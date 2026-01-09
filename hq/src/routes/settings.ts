import type { FastifyInstance, FastifyPluginOptions } from 'fastify';
import type { SettingsOperations } from '../service/settings/index.js';
import { requireAuth } from '../auth/index.js';

export interface SettingsRouteDependencies {
  operations: SettingsOperations;
}

const scopeTypeEnum = ['global', 'guild', 'user', 'perGuildUser', 'admin'];
const settingsKindEnum = ['user', 'guild', 'admin'];

const errorResponseSchema = {
  type: 'object',
  properties: {
    error: { type: 'string' },
    message: { type: 'string' },
  },
  required: ['error', 'message'],
} as const;

const sourceKindEnum = ['default', 'entry', 'merged'];

const settingValueSchema = {
  type: 'object',
  properties: {
    value: {},
    source: {
      type: 'object',
      properties: {
        kind: { type: 'string', enum: sourceKindEnum },
        scope: {
          type: 'object',
          properties: {
            kind: { type: 'string', enum: settingsKindEnum },
            scope: { type: 'string' },
            guildId: { type: 'string' },
            userId: { type: 'string' },
          },
        },
        isImportant: { type: 'boolean' },
        scopeCount: { type: 'number' },
      },
      required: ['kind'],
    },
  },
  required: ['value', 'source'],
} as const;

const settingEntrySchema = {
  type: 'object',
  properties: {
    keyId: { type: 'string' },
    value: {},
    scope: {
      type: 'object',
      properties: {
        kind: { type: 'string', enum: settingsKindEnum },
        type: { type: 'string' },
        guildId: { type: 'string' },
        userId: { type: 'string' },
      },
      required: ['kind', 'type'],
    },
    isDefault: { type: 'boolean' },
  },
  required: ['keyId', 'value', 'scope', 'isDefault'],
} as const;

export function settingsRoutes(deps: SettingsRouteDependencies) {
  const { operations } = deps;

  return async function (
    fastify: FastifyInstance,
    _opts: FastifyPluginOptions
  ) {
    fastify.get<{
      Params: { keyId: string };
      Querystring: { userId?: string; guildId?: string };
    }>(
      '/settings/:keyId',
      {
        schema: {
          tags: ['settings'],
          summary: 'Get a setting value',
          description:
            'Retrieves the resolved value for a setting key, considering scope cascading.',
          params: {
            type: 'object',
            properties: {
              keyId: {
                type: 'string',
                description: 'The setting key identifier',
              },
            },
            required: ['keyId'],
          },
          querystring: {
            type: 'object',
            properties: {
              userId: {
                type: 'string',
                description: 'User ID for user-scoped settings',
              },
              guildId: {
                type: 'string',
                description: 'Guild ID for guild-scoped settings',
              },
            },
          },
          response: {
            200: settingValueSchema,
            400: errorResponseSchema,
            401: errorResponseSchema,
            403: errorResponseSchema,
            404: errorResponseSchema,
          },
          security: [{ bearerAuth: [] }],
        },
      },
      async (request, reply) => {
        const auth = requireAuth(request, reply);
        if (!auth) return;

        const result = await operations.get(
          {
            keyId: request.params.keyId,
            userId: request.query.userId,
            guildId: request.query.guildId,
          },
          auth
        );

        if (!result.ok) {
          const statusCode = result.error.includes('not found')
            ? 404
            : result.error.includes('denied')
            ? 403
            : 400;
          return reply.status(statusCode).send({
            error: statusCode === 404 ? 'Not Found' : statusCode === 403 ? 'Forbidden' : 'Bad Request',
            message: result.error,
          });
        }

        return reply.send(result.value);
      }
    );

    fastify.put<{
      Params: { keyId: string };
      Body: {
        value: unknown;
        scopeType: string;
        userId?: string;
        guildId?: string;
      };
    }>(
      '/settings/:keyId',
      {
        schema: {
          tags: ['settings'],
          summary: 'Set a setting value',
          description: 'Sets a value for a setting key at the specified scope.',
          params: {
            type: 'object',
            properties: {
              keyId: {
                type: 'string',
                description: 'The setting key identifier',
              },
            },
            required: ['keyId'],
          },
          body: {
            type: 'object',
            properties: {
              value: {
                description: 'The value to set',
              },
              scopeType: {
                type: 'string',
                enum: scopeTypeEnum,
                description: 'The scope type to set the value at',
              },
              userId: {
                type: 'string',
                description: 'User ID for user-scoped settings',
              },
              guildId: {
                type: 'string',
                description: 'Guild ID for guild-scoped settings',
              },
            },
            required: ['value', 'scopeType'],
          },
          response: {
            204: { type: 'null', description: 'Setting updated successfully' },
            400: errorResponseSchema,
            401: errorResponseSchema,
            403: errorResponseSchema,
            404: errorResponseSchema,
          },
          security: [{ bearerAuth: [] }],
        },
      },
      async (request, reply) => {
        const auth = requireAuth(request, reply);
        if (!auth) return;

        const result = await operations.set(
          {
            keyId: request.params.keyId,
            value: request.body.value,
            scopeType: request.body.scopeType as Parameters<
              typeof operations.set
            >[0]['scopeType'],
            userId: request.body.userId,
            guildId: request.body.guildId,
          },
          auth
        );

        if (!result.ok) {
          const statusCode = result.error.includes('not found')
            ? 404
            : result.error.includes('denied')
            ? 403
            : 400;
          return reply.status(statusCode).send({
            error: statusCode === 404 ? 'Not Found' : statusCode === 403 ? 'Forbidden' : 'Bad Request',
            message: result.error,
          });
        }

        return reply.status(204).send();
      }
    );

    fastify.delete<{
      Params: { keyId: string };
      Querystring: {
        scopeType: string;
        userId?: string;
        guildId?: string;
      };
    }>(
      '/settings/:keyId',
      {
        schema: {
          tags: ['settings'],
          summary: 'Delete a setting value',
          description:
            'Removes a setting value at the specified scope, reverting to the next level in the cascade.',
          params: {
            type: 'object',
            properties: {
              keyId: {
                type: 'string',
                description: 'The setting key identifier',
              },
            },
            required: ['keyId'],
          },
          querystring: {
            type: 'object',
            properties: {
              scopeType: {
                type: 'string',
                enum: scopeTypeEnum,
                description: 'The scope type to delete from',
              },
              userId: {
                type: 'string',
                description: 'User ID for user-scoped settings',
              },
              guildId: {
                type: 'string',
                description: 'Guild ID for guild-scoped settings',
              },
            },
            required: ['scopeType'],
          },
          response: {
            200: {
              type: 'object',
              properties: {
                deleted: { type: 'boolean' },
              },
              required: ['deleted'],
            },
            400: errorResponseSchema,
            401: errorResponseSchema,
            403: errorResponseSchema,
            404: errorResponseSchema,
          },
          security: [{ bearerAuth: [] }],
        },
      },
      async (request, reply) => {
        const auth = requireAuth(request, reply);
        if (!auth) return;

        const result = await operations.delete(
          {
            keyId: request.params.keyId,
            scopeType: request.query.scopeType as Parameters<
              typeof operations.delete
            >[0]['scopeType'],
            userId: request.query.userId,
            guildId: request.query.guildId,
          },
          auth
        );

        if (!result.ok) {
          const statusCode = result.error.includes('not found')
            ? 404
            : result.error.includes('denied')
            ? 403
            : 400;
          return reply.status(statusCode).send({
            error: statusCode === 404 ? 'Not Found' : statusCode === 403 ? 'Forbidden' : 'Bad Request',
            message: result.error,
          });
        }

        return reply.send({ deleted: result.value });
      }
    );

    fastify.get<{
      Querystring: {
        settingsKind: string;
        scopeType?: string;
        userId?: string;
        guildId?: string;
      };
    }>(
      '/settings',
      {
        schema: {
          tags: ['settings'],
          summary: 'List settings',
          description:
            'Lists all settings for the specified kind and optional scope filter. Only accessible by admins or the resource owner.',
          querystring: {
            type: 'object',
            properties: {
              settingsKind: {
                type: 'string',
                enum: settingsKindEnum,
                description: 'The kind of settings to list',
              },
              scopeType: {
                type: 'string',
                enum: scopeTypeEnum,
                description: 'Filter by scope type',
              },
              userId: {
                type: 'string',
                description: 'User ID for user settings',
              },
              guildId: {
                type: 'string',
                description: 'Guild ID for guild settings',
              },
            },
            required: ['settingsKind'],
          },
          response: {
            200: {
              type: 'object',
              properties: {
                entries: {
                  type: 'array',
                  items: settingEntrySchema,
                },
              },
              required: ['entries'],
            },
            400: errorResponseSchema,
            401: errorResponseSchema,
            403: errorResponseSchema,
          },
          security: [{ bearerAuth: [] }],
        },
      },
      async (request, reply) => {
        const auth = requireAuth(request, reply);
        if (!auth) return;

        const result = await operations.list(
          {
            settingsKind: request.query.settingsKind as Parameters<
              typeof operations.list
            >[0]['settingsKind'],
            scopeType: request.query.scopeType as
              | Parameters<typeof operations.list>[0]['scopeType']
              | undefined,
            userId: request.query.userId,
            guildId: request.query.guildId,
          },
          auth
        );

        if (!result.ok) {
          const statusCode = result.error.includes('denied') ? 403 : 400;
          return reply.status(statusCode).send({
            error: statusCode === 403 ? 'Forbidden' : 'Bad Request',
            message: result.error,
          });
        }

        return reply.send({ entries: result.value });
      }
    );
  };
}
