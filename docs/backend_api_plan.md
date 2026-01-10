# Backend API Implementation Plan for `hq/`

## Overview

This document provides a comprehensive plan for implementing a backend REST API in the `hq/` directory that serves the web application while also being accessible from a Discord bot through a wrapping layer.

---

## Architecture Summary

### Current State
- **HQ Service**: Basic Fastify server with settings management, Discord bot integration, JWT auth, database (Drizzle ORM + PostgreSQL), Redis cache, OpenTelemetry tracing
- **Web App**: React SPA consuming REST API endpoints (documented in `api_endpoints.md`)
- **Data Schemas**: Shared Zod schemas in `@zako-ac/zako3-data` package

### Target Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    HQ Service                            │
├─────────────────────────────────────────────────────────┤
│                                                          │
│  ┌──────────────┐        ┌──────────────────────────┐   │
│  │              │        │                          │   │
│  │  Fastify     │        │  Service Layer           │   │
│  │  REST API    │───────▶│  (Business Logic)        │   │
│  │              │        │                          │   │
│  └──────────────┘        └──────────────────────────┘   │
│         │                            │                   │
│         │                            │                   │
│         ▼                            ▼                   │
│  ┌──────────────┐        ┌──────────────────────────┐   │
│  │              │        │                          │   │
│  │  Discord     │───────▶│  Repository Layer        │   │
│  │  Bot API     │        │  (Data Access)           │   │
│  │  Wrapper     │        │                          │   │
│  └──────────────┘        └──────────────────────────┘   │
│                                      │                   │
│                                      ▼                   │
│                          ┌──────────────────────────┐   │
│                          │  Database (PostgreSQL)   │   │
│                          │  Redis Cache             │   │
│                          └──────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
```

**Key Design Principles:**
1. **Service Layer**: Pure business logic, transport-agnostic
2. **Repository Layer**: Database access and caching
3. **Transport Layer**: Fastify REST API and Discord bot commands both consume the service layer
4. **Shared Validation**: Use `@zako-ac/zako3-data` Zod schemas everywhere

---

## Phase 1: Foundation Setup

### 1.1 Add Dependencies

**File**: `hq/package.json`

Add the following dependencies:

```json
{
  "dependencies": {
    "@zako-ac/zako3-data": "file:../packages/zako3-data",
    "@fastify/cors": "^10.0.2",
    "@fastify/cookie": "^11.0.2",
    "@fastify/sensible": "^6.0.2",
    "nanoid": "^5.0.9",
    "bcryptjs": "^2.4.3",
    "date-fns": "^4.1.0"
  },
  "devDependencies": {
    "@types/bcryptjs": "^2.4.6"
  }
}
```

**Run**: `pnpm install`

### 1.2 Create Directory Structure

```bash
hq/src/
├── api/                     # Fastify REST API layer
│   ├── routes/              # Route handlers
│   │   ├── auth.ts          # Auth routes
│   │   ├── users.ts         # User routes
│   │   ├── taps.ts          # Tap routes
│   │   ├── notifications.ts # Notification routes
│   │   └── admin.ts         # Admin routes
│   ├── middlewares/         # API-specific middleware
│   │   ├── auth.ts          # Authentication
│   │   ├── admin.ts         # Admin authorization
│   │   └── validation.ts    # Request validation
│   ├── schemas/             # API request/response schemas
│   │   └── index.ts         # Re-export from @zako-ac/zako3-data
│   └── index.ts             # Route registration
│
├── services/                # Business logic (transport-agnostic)
│   ├── auth/
│   │   ├── service.ts       # Auth business logic
│   │   ├── oauth.ts         # Discord OAuth flow
│   │   └── tokens.ts        # JWT token management
│   ├── users/
│   │   ├── service.ts       # User management
│   │   └── permissions.ts   # User permissions
│   ├── taps/
│   │   ├── service.ts       # Tap CRUD
│   │   ├── permissions.ts   # Tap access control
│   │   ├── verification.ts  # Tap verification
│   │   ├── stats.ts         # Tap statistics
│   │   └── tokens.ts        # API token management
│   ├── notifications/
│   │   ├── service.ts       # Notification management
│   │   └── delivery.ts      # Notification delivery
│   ├── admin/
│   │   ├── service.ts       # Admin operations
│   │   ├── moderation.ts    # User moderation
│   │   └── verification.ts  # Verification management
│   └── index.ts             # Service factory/registry
│
├── repositories/            # Data access layer
│   ├── users.ts             # User repository
│   ├── taps.ts              # Tap repository
│   ├── notifications.ts     # Notification repository
│   ├── audit-logs.ts        # Audit log repository
│   ├── verifications.ts     # Verification repository
│   ├── api-tokens.ts        # API token repository
│   └── index.ts             # Repository factory
│
├── db/
│   ├── schema/              # Drizzle schemas
│   │   ├── users.ts         # Users table
│   │   ├── taps.ts          # Taps table
│   │   ├── notifications.ts # Notifications table
│   │   ├── audit-logs.ts    # Audit logs table
│   │   ├── verifications.ts # Verification requests table
│   │   ├── api-tokens.ts    # API tokens table
│   │   └── index.ts         # Schema exports
│   ├── migrations/          # Database migrations
│   └── index.ts             # Database connection
│
├── bot/                     # Discord bot integration
│   ├── commands/            # Bot commands
│   │   ├── taps.ts          # Tap management commands
│   │   ├── admin.ts         # Admin commands
│   │   └── index.ts         # Command registry
│   ├── wrapper.ts           # Service layer wrapper for bot
│   └── bot.ts               # Bot setup
│
├── lib/                     # Shared utilities
│   ├── errors.ts            # Custom error classes
│   ├── result.ts            # Result type (ok/error)
│   ├── pagination.ts        # Pagination helpers
│   ├── validation.ts        # Validation helpers
│   └── crypto.ts            # Crypto utilities
│
├── types/                   # TypeScript types
│   ├── context.ts           # Request context
│   └── services.ts          # Service interfaces
│
└── index.ts                 # Application entry point
```

---

## Phase 2: Database Schema Design

### 2.1 Database Tables

Based on `data_schemas.md`, create Drizzle schemas:

#### **Users Table** (`hq/src/db/schema/users.ts`)

```typescript
import { pgTable, text, boolean, timestamp, varchar } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';

export const users = pgTable('users', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  discordId: varchar('discord_id', { length: 32 }).notNull().unique(),
  username: varchar('username', { length: 100 }).notNull(),
  avatar: text('avatar').notNull(),
  email: varchar('email', { length: 255 }),
  isAdmin: boolean('is_admin').notNull().default(false),
  isBanned: boolean('is_banned').notNull().default(false),
  banReason: text('ban_reason'),
  banExpiresAt: timestamp('ban_expires_at'),
  lastActiveAt: timestamp('last_active_at'),
  createdAt: timestamp('created_at').notNull().defaultNow(),
  updatedAt: timestamp('updated_at').notNull().defaultNow(),
});

export type User = typeof users.$inferSelect;
export type NewUser = typeof users.$inferInsert;
```

#### **Taps Table** (`hq/src/db/schema/taps.ts`)

```typescript
import { pgTable, text, timestamp, varchar, integer, jsonb } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';
import { users } from './users';
import type { TapPermissionConfig } from '@zako-ac/zako3-data';

export const taps = pgTable('taps', {
  id: varchar('id', { length: 32 }).primaryKey(), // User-defined
  name: varchar('name', { length: 64 }).notNull(),
  description: text('description').notNull().default(''),
  ownerId: text('owner_id').notNull().references(() => users.id),
  occupation: varchar('occupation', { length: 20 }).notNull().default('base'), // 'official' | 'verified' | 'base'
  roles: jsonb('roles').notNull().$type<string[]>(), // ['music', 'tts']
  permission: jsonb('permission').notNull().$type<TapPermissionConfig>(),
  totalUses: integer('total_uses').notNull().default(0),
  createdAt: timestamp('created_at').notNull().defaultNow(),
  updatedAt: timestamp('updated_at').notNull().defaultNow(),
});

export type Tap = typeof taps.$inferSelect;
export type NewTap = typeof taps.$inferInsert;
```

#### **Notifications Table** (`hq/src/db/schema/notifications.ts`)

```typescript
import { pgTable, text, timestamp, boolean, varchar, jsonb } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';
import { users } from './users';

export const notifications = pgTable('notifications', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  userId: text('user_id').notNull().references(() => users.id),
  category: varchar('category', { length: 50 }).notNull(),
  level: varchar('level', { length: 20 }).notNull(), // 'info' | 'success' | 'warning' | 'error'
  title: varchar('title', { length: 255 }).notNull(),
  message: text('message').notNull(),
  metadata: jsonb('metadata').$type<Record<string, unknown>>(),
  isRead: boolean('is_read').notNull().default(false),
  createdAt: timestamp('created_at').notNull().defaultNow(),
});

export type Notification = typeof notifications.$inferSelect;
export type NewNotification = typeof notifications.$inferInsert;
```

#### **Tap Audit Logs Table** (`hq/src/db/schema/audit-logs.ts`)

```typescript
import { pgTable, text, timestamp, varchar } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';
import { taps } from './taps';
import { users } from './users';

export const tapAuditLogs = pgTable('tap_audit_logs', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  tapId: varchar('tap_id', { length: 32 }).notNull().references(() => taps.id, { onDelete: 'cascade' }),
  actorId: text('actor_id').references(() => users.id),
  action: varchar('action', { length: 100 }).notNull(),
  details: text('details'),
  createdAt: timestamp('created_at').notNull().defaultNow(),
});

export type TapAuditLog = typeof tapAuditLogs.$inferSelect;
export type NewTapAuditLog = typeof tapAuditLogs.$inferInsert;
```

#### **Verification Requests Table** (`hq/src/db/schema/verifications.ts`)

```typescript
import { pgTable, text, timestamp, varchar } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';
import { taps } from './taps';
import { users } from './users';

export const verificationRequests = pgTable('verification_requests', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  tapId: varchar('tap_id', { length: 32 }).notNull().references(() => taps.id, { onDelete: 'cascade' }),
  requesterId: text('requester_id').notNull().references(() => users.id),
  reason: text('reason').notNull(),
  evidence: text('evidence'),
  status: varchar('status', { length: 20 }).notNull().default('pending'), // 'pending' | 'approved' | 'rejected'
  reviewedBy: text('reviewed_by').references(() => users.id),
  rejectionReason: text('rejection_reason'),
  requestedAt: timestamp('requested_at').notNull().defaultNow(),
  reviewedAt: timestamp('reviewed_at'),
});

export type VerificationRequest = typeof verificationRequests.$inferSelect;
export type NewVerificationRequest = typeof verificationRequests.$inferInsert;
```

#### **API Tokens Table** (`hq/src/db/schema/api-tokens.ts`)

```typescript
import { pgTable, text, timestamp, varchar } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';
import { taps } from './taps';

export const apiTokens = pgTable('api_tokens', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  tapId: varchar('tap_id', { length: 32 }).notNull().references(() => taps.id, { onDelete: 'cascade' }),
  label: varchar('label', { length: 64 }).notNull(),
  tokenHash: text('token_hash').notNull(), // bcrypt hash
  tokenPreview: varchar('token_preview', { length: 20 }).notNull(), // Last 8 chars for display
  lastUsedAt: timestamp('last_used_at'),
  expiresAt: timestamp('expires_at'),
  createdAt: timestamp('created_at').notNull().defaultNow(),
});

export type ApiToken = typeof apiTokens.$inferSelect;
export type NewApiToken = typeof apiTokens.$inferInsert;
```

#### **Admin Activity Table** (`hq/src/db/schema/admin-activity.ts`)

```typescript
import { pgTable, text, timestamp, varchar } from 'drizzle-orm/pg-core';
import { createId } from '@paralleldrive/cuid2';
import { users } from './users';

export const adminActivity = pgTable('admin_activity', {
  id: text('id').primaryKey().$defaultFn(() => createId()),
  adminId: text('admin_id').notNull().references(() => users.id),
  action: varchar('action', { length: 100 }).notNull(),
  targetType: varchar('target_type', { length: 20 }).notNull(), // 'user' | 'tap' | 'notification' | 'system'
  targetId: text('target_id').notNull(),
  targetName: varchar('target_name', { length: 255 }).notNull(),
  details: text('details'),
  timestamp: timestamp('timestamp').notNull().defaultNow(),
});

export type AdminActivity = typeof adminActivity.$inferSelect;
export type NewAdminActivity = typeof adminActivity.$inferInsert;
```

### 2.2 Generate Migration

```bash
pnpm db:generate
pnpm db:migrate
```

---

## Phase 3: Repository Layer

Create data access layer with caching.

### 3.1 Base Repository Pattern

**File**: `hq/src/repositories/base.ts`

```typescript
import type { Redis } from 'ioredis';
import type { Logger } from 'pino';
import type { Result } from '../lib/result';

export interface CacheOptions {
  ttl?: number; // seconds
  key: string;
}

export abstract class BaseRepository {
  constructor(
    protected readonly redis: Redis,
    protected readonly logger: Logger
  ) {}

  protected async cache<T>(
    options: CacheOptions,
    fetcher: () => Promise<Result<T>>
  ): Promise<Result<T>> {
    // Try cache first
    const cached = await this.redis.get(options.key);
    if (cached) {
      try {
        return { ok: true, value: JSON.parse(cached) };
      } catch {
        // Invalid cache, continue to fetcher
      }
    }

    // Fetch fresh data
    const result = await fetcher();
    if (result.ok && options.ttl) {
      await this.redis.setex(options.key, options.ttl, JSON.stringify(result.value));
    }

    return result;
  }

  protected async invalidateCache(key: string | string[]): Promise<void> {
    const keys = Array.isArray(key) ? key : [key];
    if (keys.length > 0) {
      await this.redis.del(...keys);
    }
  }
}
```

### 3.2 User Repository

**File**: `hq/src/repositories/users.ts`

```typescript
import { eq, and, or, like, desc, asc } from 'drizzle-orm';
import type { PostgresJsDatabase } from 'drizzle-orm/postgres-js';
import type { Redis } from 'ioredis';
import type { Logger } from 'pino';
import { users, type User, type NewUser } from '../db/schema/users';
import type { PaginationParams, UserFilters, UserSort } from '@zako-ac/zako3-data';
import type { Result } from '../lib/result';
import { BaseRepository } from './base';

export class UserRepository extends BaseRepository {
  constructor(
    private readonly db: PostgresJsDatabase,
    redis: Redis,
    logger: Logger
  ) {
    super(redis, logger);
  }

  async create(data: NewUser): Promise<Result<User>> {
    try {
      const [user] = await this.db.insert(users).values(data).returning();
      await this.invalidateCache(`user:discord:${data.discordId}`);
      return { ok: true, value: user };
    } catch (error) {
      this.logger.error({ error }, 'Failed to create user');
      return { ok: false, error: 'Failed to create user' };
    }
  }

  async findById(id: string): Promise<Result<User | null>> {
    return this.cache(
      { key: `user:${id}`, ttl: 300 },
      async () => {
        try {
          const [user] = await this.db.select().from(users).where(eq(users.id, id));
          return { ok: true, value: user || null };
        } catch (error) {
          this.logger.error({ error, id }, 'Failed to find user by ID');
          return { ok: false, error: 'Failed to find user' };
        }
      }
    );
  }

  async findByDiscordId(discordId: string): Promise<Result<User | null>> {
    return this.cache(
      { key: `user:discord:${discordId}`, ttl: 300 },
      async () => {
        try {
          const [user] = await this.db.select().from(users).where(eq(users.discordId, discordId));
          return { ok: true, value: user || null };
        } catch (error) {
          this.logger.error({ error, discordId }, 'Failed to find user by Discord ID');
          return { ok: false, error: 'Failed to find user' };
        }
      }
    );
  }

  async update(id: string, data: Partial<User>): Promise<Result<User>> {
    try {
      const [user] = await this.db
        .update(users)
        .set({ ...data, updatedAt: new Date() })
        .where(eq(users.id, id))
        .returning();
      
      await this.invalidateCache([`user:${id}`, `user:discord:${user.discordId}`]);
      return { ok: true, value: user };
    } catch (error) {
      this.logger.error({ error, id }, 'Failed to update user');
      return { ok: false, error: 'Failed to update user' };
    }
  }

  async list(
    pagination: PaginationParams,
    filters?: UserFilters,
    sort?: UserSort
  ): Promise<Result<{ users: User[]; total: number }>> {
    try {
      const conditions = [];
      
      if (filters?.search) {
        conditions.push(like(users.username, `%${filters.search}%`));
      }
      if (filters?.isBanned !== undefined) {
        conditions.push(eq(users.isBanned, filters.isBanned));
      }
      if (filters?.isAdmin !== undefined) {
        conditions.push(eq(users.isAdmin, filters.isAdmin));
      }

      const where = conditions.length > 0 ? and(...conditions) : undefined;

      const [total] = await this.db
        .select({ count: count() })
        .from(users)
        .where(where);

      const orderBy = sort
        ? sort.direction === 'asc'
          ? asc(users[sort.field])
          : desc(users[sort.field])
        : desc(users.createdAt);

      const userList = await this.db
        .select()
        .from(users)
        .where(where)
        .orderBy(orderBy)
        .limit(pagination.perPage)
        .offset((pagination.page - 1) * pagination.perPage);

      return { ok: true, value: { users: userList, total: total.count } };
    } catch (error) {
      this.logger.error({ error }, 'Failed to list users');
      return { ok: false, error: 'Failed to list users' };
    }
  }
}
```

*(Similar patterns for `TapRepository`, `NotificationRepository`, etc.)*

---

## Phase 4: Service Layer (Business Logic)

### 4.1 Service Interface Pattern

**File**: `hq/src/types/services.ts`

```typescript
import type { Result } from '../lib/result';
import type { User, Tap, /* ... other types */ } from '@zako-ac/zako3-data';

export interface IUserService {
  getUser(userId: string): Promise<Result<User>>;
  getCurrentUser(userId: string): Promise<Result<User>>;
  updateLastActive(userId: string): Promise<Result<void>>;
  // ... more methods
}

export interface ITapService {
  getTap(tapId: string, userId?: string): Promise<Result<Tap>>;
  createTap(data: CreateTapInput, userId: string): Promise<Result<Tap>>;
  updateTap(tapId: string, data: UpdateTapInput, userId: string): Promise<Result<Tap>>;
  deleteTap(tapId: string, userId: string): Promise<Result<void>>;
  checkAccess(tapId: string, userId: string): Promise<Result<boolean>>;
  // ... more methods
}

// ... more service interfaces
```

### 4.2 Tap Service Implementation

**File**: `hq/src/services/taps/service.ts`

```typescript
import type { Logger } from 'pino';
import type { ITapService } from '../../types/services';
import type { TapRepository } from '../../repositories/taps';
import type { UserRepository } from '../../repositories/users';
import type { TapAuditLogRepository } from '../../repositories/audit-logs';
import type { Result } from '../../lib/result';
import type { CreateTapInput, UpdateTapInput, Tap } from '@zako-ac/zako3-data';
import { createTapSchema, updateTapSchema } from '@zako-ac/zako3-data';

export class TapService implements ITapService {
  constructor(
    private readonly tapRepo: TapRepository,
    private readonly userRepo: UserRepository,
    private readonly auditRepo: TapAuditLogRepository,
    private readonly logger: Logger
  ) {}

  async createTap(data: CreateTapInput, userId: string): Promise<Result<Tap>> {
    // Validate input
    const validation = createTapSchema.safeParse(data);
    if (!validation.success) {
      return { ok: false, error: validation.error.message };
    }

    // Check if tap ID is already taken
    const existing = await this.tapRepo.findById(data.id);
    if (existing.ok && existing.value) {
      return { ok: false, error: 'Tap ID already exists' };
    }

    // Create tap
    const result = await this.tapRepo.create({
      ...data,
      ownerId: userId,
      occupation: 'base',
      totalUses: 0,
    });

    if (result.ok) {
      // Log audit entry
      await this.auditRepo.create({
        tapId: data.id,
        actorId: userId,
        action: 'tap_created',
        details: JSON.stringify({ name: data.name }),
      });
    }

    return result;
  }

  async getTap(tapId: string, userId?: string): Promise<Result<Tap>> {
    const result = await this.tapRepo.findById(tapId);
    if (!result.ok) {
      return result;
    }

    if (!result.value) {
      return { ok: false, error: 'Tap not found' };
    }

    // Check access if user is provided
    if (userId) {
      const hasAccess = await this.checkAccess(tapId, userId);
      if (!hasAccess.ok || !hasAccess.value) {
        return { ok: false, error: 'Access denied' };
      }
    }

    return result;
  }

  async checkAccess(tapId: string, userId: string): Promise<Result<boolean>> {
    const tapResult = await this.tapRepo.findById(tapId);
    if (!tapResult.ok || !tapResult.value) {
      return { ok: false, error: 'Tap not found' };
    }

    const tap = tapResult.value;

    // Owner always has access
    if (tap.ownerId === userId) {
      return { ok: true, value: true };
    }

    // Check permission config
    switch (tap.permission.type) {
      case 'owner_only':
        return { ok: true, value: false };
      case 'public':
        return { ok: true, value: true };
      case 'whitelisted':
        return { ok: true, value: tap.permission.userIds.includes(userId) };
      case 'blacklisted':
        return { ok: true, value: !tap.permission.userIds.includes(userId) };
      default:
        return { ok: true, value: false };
    }
  }

  // ... more methods
}
```

---

## Phase 5: Fastify REST API Layer

### 5.1 Route Handler Pattern

**File**: `hq/src/api/routes/taps.ts`

```typescript
import type { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify';
import type { ITapService } from '../../types/services';
import type { AuthContext } from '../../types/context';
import { createTapSchema, updateTapSchema } from '@zako-ac/zako3-data';

interface TapRoutesDeps {
  tapService: ITapService;
}

export async function tapRoutes(fastify: FastifyInstance, deps: TapRoutesDeps) {
  const { tapService } = deps;

  // GET /taps - List taps
  fastify.get('/', async (req: FastifyRequest, reply: FastifyReply) => {
    const { page = 1, perPage = 20, ...filters } = req.query as any;
    
    const result = await tapService.listTaps(
      { page, perPage },
      filters,
      req.auth?.userId
    );

    if (!result.ok) {
      return reply.code(500).send({ error: result.error });
    }

    return reply.send({
      data: result.value.taps,
      meta: {
        total: result.value.total,
        page,
        perPage,
        totalPages: Math.ceil(result.value.total / perPage),
      },
    });
  });

  // POST /taps - Create tap
  fastify.post('/', {
    schema: {
      body: createTapSchema,
      response: {
        201: { type: 'object' }, // Add proper schema
      },
    },
  }, async (req: FastifyRequest, reply: FastifyReply) => {
    const auth = req.auth as AuthContext;
    
    const result = await tapService.createTap(req.body as any, auth.userId);

    if (!result.ok) {
      return reply.code(400).send({ error: result.error });
    }

    return reply.code(201).send({ data: result.value });
  });

  // GET /taps/:tapId - Get tap
  fastify.get('/:tapId', async (req: FastifyRequest, reply: FastifyReply) => {
    const { tapId } = req.params as { tapId: string };
    
    const result = await tapService.getTap(tapId, req.auth?.userId);

    if (!result.ok) {
      return reply.code(404).send({ error: result.error });
    }

    return reply.send({ data: result.value });
  });

  // PATCH /taps/:tapId - Update tap
  fastify.patch('/:tapId', {
    schema: {
      body: updateTapSchema,
    },
  }, async (req: FastifyRequest, reply: FastifyReply) => {
    const { tapId } = req.params as { tapId: string };
    const auth = req.auth as AuthContext;
    
    const result = await tapService.updateTap(tapId, req.body as any, auth.userId);

    if (!result.ok) {
      return reply.code(403).send({ error: result.error });
    }

    return reply.send({ data: result.value });
  });

  // DELETE /taps/:tapId - Delete tap
  fastify.delete('/:tapId', async (req: FastifyRequest, reply: FastifyReply) => {
    const { tapId } = req.params as { tapId: string };
    const auth = req.auth as AuthContext;
    
    const result = await tapService.deleteTap(tapId, auth.userId);

    if (!result.ok) {
      return reply.code(403).send({ error: result.error });
    }

    return reply.code(204).send();
  });

  // ... more routes
}
```

### 5.2 API Registration

**File**: `hq/src/api/index.ts`

```typescript
import type { FastifyInstance } from 'fastify';
import { authRoutes } from './routes/auth';
import { userRoutes } from './routes/users';
import { tapRoutes } from './routes/taps';
import { notificationRoutes } from './routes/notifications';
import { adminRoutes } from './routes/admin';

export interface ApiDependencies {
  services: {
    auth: IAuthService;
    user: IUserService;
    tap: ITapService;
    notification: INotificationService;
    admin: IAdminService;
  };
}

export async function registerApi(fastify: FastifyInstance, deps: ApiDependencies) {
  await fastify.register(authRoutes, { prefix: '/auth', ...deps });
  await fastify.register(userRoutes, { prefix: '/users', ...deps });
  await fastify.register(tapRoutes, { prefix: '/taps', ...deps });
  await fastify.register(notificationRoutes, { prefix: '/notifications', ...deps });
  await fastify.register(adminRoutes, { prefix: '/admin', ...deps });
}
```

---

## Phase 6: Discord Bot Wrapper

### 6.1 Service Wrapper for Bot

**File**: `hq/src/bot/wrapper.ts`

```typescript
import type { ITapService, IUserService } from '../types/services';
import type { Result } from '../lib/result';

/**
 * Wrapper around services that provides a simpler interface for Discord bot commands
 */
export class BotServiceWrapper {
  constructor(
    private readonly tapService: ITapService,
    private readonly userService: IUserService
  ) {}

  /**
   * Get or create user from Discord ID
   */
  async getOrCreateUser(discordId: string, username: string, avatar: string) {
    const existing = await this.userService.findByDiscordId(discordId);
    if (existing.ok && existing.value) {
      return existing;
    }

    return this.userService.create({
      discordId,
      username,
      avatar,
    });
  }

  /**
   * List user's taps
   */
  async getUserTaps(discordId: string): Promise<Result<Tap[]>> {
    const userResult = await this.userService.findByDiscordId(discordId);
    if (!userResult.ok || !userResult.value) {
      return { ok: false, error: 'User not found' };
    }

    return this.tapService.listUserTaps(userResult.value.id);
  }

  /**
   * Create tap via bot command
   */
  async createTapFromBot(discordId: string, data: CreateTapInput): Promise<Result<Tap>> {
    const userResult = await this.getOrCreateUser(discordId, 'Unknown', '');
    if (!userResult.ok || !userResult.value) {
      return { ok: false, error: 'Failed to get user' };
    }

    return this.tapService.createTap(data, userResult.value.id);
  }

  // ... more bot-friendly methods
}
```

### 6.2 Bot Commands

**File**: `hq/src/bot/commands/taps.ts`

```typescript
import { SlashCommandBuilder } from 'discord.js';
import type { BotServiceWrapper } from '../wrapper';

export function createTapCommands(wrapper: BotServiceWrapper) {
  return {
    data: new SlashCommandBuilder()
      .setName('tap')
      .setDescription('Manage your taps')
      .addSubcommand(subcommand =>
        subcommand
          .setName('create')
          .setDescription('Create a new tap')
          .addStringOption(option =>
            option.setName('id').setDescription('Tap ID').setRequired(true)
          )
          .addStringOption(option =>
            option.setName('name').setDescription('Tap name').setRequired(true)
          )
      )
      .addSubcommand(subcommand =>
        subcommand.setName('list').setDescription('List your taps')
      ),

    async execute(interaction: any) {
      const subcommand = interaction.options.getSubcommand();

      if (subcommand === 'create') {
        const id = interaction.options.getString('id');
        const name = interaction.options.getString('name');

        const result = await wrapper.createTapFromBot(interaction.user.id, {
          id,
          name,
          description: '',
          roles: ['music'],
          permission: { type: 'owner_only' },
        });

        if (!result.ok) {
          return interaction.reply({ content: `Error: ${result.error}`, ephemeral: true });
        }

        return interaction.reply({ content: `Tap "${name}" created!`, ephemeral: true });
      }

      if (subcommand === 'list') {
        const result = await wrapper.getUserTaps(interaction.user.id);

        if (!result.ok) {
          return interaction.reply({ content: `Error: ${result.error}`, ephemeral: true });
        }

        const taps = result.value;
        const list = taps.map(t => `- ${t.id}: ${t.name}`).join('\n') || 'No taps found';
        
        return interaction.reply({ content: `Your taps:\n${list}`, ephemeral: true });
      }
    },
  };
}
```

---

## Phase 7: Implementation Checklist

### 7.1 Database & Repositories
- [ ] Create all database schemas
- [ ] Generate and run migrations
- [ ] Implement BaseRepository with caching
- [ ] Implement UserRepository
- [ ] Implement TapRepository
- [ ] Implement NotificationRepository
- [ ] Implement AuditLogRepository
- [ ] Implement VerificationRepository
- [ ] Implement ApiTokenRepository
- [ ] Add indexes for performance

### 7.2 Services
- [ ] Create Result type helper
- [ ] Create error classes
- [ ] Implement UserService
- [ ] Implement TapService with permissions
- [ ] Implement AuthService with OAuth
- [ ] Implement NotificationService
- [ ] Implement AdminService
- [ ] Implement TapStatsService
- [ ] Implement TapTokenService
- [ ] Add service tests

### 7.3 API Layer
- [ ] Set up CORS and security middleware
- [ ] Implement auth middleware
- [ ] Implement admin middleware
- [ ] Create auth routes (/auth/*)
- [ ] Create user routes (/users/*)
- [ ] Create tap routes (/taps/*)
- [ ] Create notification routes (/notifications/*)
- [ ] Create admin routes (/admin/*)
- [ ] Add Swagger documentation
- [ ] Add request validation
- [ ] Add error handling

### 7.4 Bot Integration
- [ ] Create BotServiceWrapper
- [ ] Implement tap commands
- [ ] Implement admin commands
- [ ] Add bot error handling
- [ ] Test bot <-> service integration

### 7.5 Testing & Documentation
- [ ] Write unit tests for services
- [ ] Write integration tests for API
- [ ] Add API endpoint documentation
- [ ] Add environment variable docs
- [ ] Create deployment guide

---

## Phase 8: Key Implementation Details

### 8.1 Authentication Flow

```
1. User clicks "Login with Discord" on web app
2. Frontend redirects to /auth/login
3. Backend generates Discord OAuth URL and returns it
4. User authorizes on Discord
5. Discord redirects to /auth/callback?code=...
6. Backend:
   - Exchanges code for Discord user info
   - Creates/updates user in database
   - Generates JWT token
   - Returns token + user data
7. Frontend stores JWT and uses for all API requests
```

### 8.2 Request Context

**File**: `hq/src/types/context.ts`

```typescript
export interface AuthContext {
  userId: string;
  isAdmin: boolean;
  discordId: string;
}

declare module 'fastify' {
  interface FastifyRequest {
    auth?: AuthContext;
  }
}
```

### 8.3 Error Handling

**File**: `hq/src/lib/errors.ts`

```typescript
export class AppError extends Error {
  constructor(
    public code: string,
    message: string,
    public statusCode: number = 500,
    public details?: Record<string, unknown>
  ) {
    super(message);
    this.name = 'AppError';
  }
}

export class NotFoundError extends AppError {
  constructor(resource: string) {
    super('NOT_FOUND', `${resource} not found`, 404);
  }
}

export class UnauthorizedError extends AppError {
  constructor(message = 'Unauthorized') {
    super('UNAUTHORIZED', message, 401);
  }
}

export class ForbiddenError extends AppError {
  constructor(message = 'Forbidden') {
    super('FORBIDDEN', message, 403);
  }
}

export class ValidationError extends AppError {
  constructor(message: string, details?: Record<string, unknown>) {
    super('VALIDATION_ERROR', message, 400, details);
  }
}
```

### 8.4 Pagination Helper

**File**: `hq/src/lib/pagination.ts`

```typescript
import type { PaginationParams, PaginationMeta } from '@zako-ac/zako3-data';

export function createPaginationMeta(
  params: PaginationParams,
  total: number
): PaginationMeta {
  return {
    total,
    page: params.page,
    perPage: params.perPage,
    totalPages: Math.ceil(total / params.perPage),
  };
}

export function parsePaginationQuery(query: any): PaginationParams {
  const page = Math.max(1, parseInt(query.page) || 1);
  const perPage = Math.min(100, Math.max(1, parseInt(query.perPage) || 20));
  return { page, perPage };
}
```

---

## Summary

This plan provides a complete blueprint for implementing the backend API in `hq/`. Key features:

1. **Layered Architecture**: Repository → Service → API/Bot
2. **Transport Agnostic**: Same business logic for REST API and Discord bot
3. **Type Safe**: Uses Zod schemas from `@zako-ac/zako3-data`
4. **Scalable**: Redis caching, proper indexing, pagination
5. **Maintainable**: Clear separation of concerns, comprehensive error handling
6. **Tested**: Unit and integration tests throughout

The implementation follows the API contract defined in `api_endpoints.md` and uses the data schemas from `data_schemas.md`, ensuring full compatibility with the web application.
