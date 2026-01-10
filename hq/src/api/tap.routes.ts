import type { FastifyInstance, FastifyPluginOptions } from 'fastify';
import type { Logger } from 'pino';
import { z } from 'zod';
import {
  TapService,
  type CreateTapRequest,
  type UpdateTapRequest,
} from '../services/tap.service.js';
import { requireAuth } from './context.js';
import { isOk } from '../lib/result.js';
import {
  NotFoundError,
  ForbiddenError,
  ValidationError,
  AppError,
} from '../lib/errors.js';

// Validation schemas
const createTapRequestSchema = z.object({
  id: z.string().min(3).max(50),
  name: z.string().min(1).max(255),
  description: z.string().max(1000).optional(),
  isPrivate: z.boolean().optional(),
});

const updateTapRequestSchema = z.object({
  name: z.string().min(1).max(255).optional(),
  description: z.string().max(1000).optional(),
  isPrivate: z.boolean().optional(),
  isLocked: z.boolean().optional(),
});

export interface TapRouteDependencies {
  tapService: TapService;
  logger: Logger;
}

/**
 * Tap API routes
 */
export function tapRoutes(deps: TapRouteDependencies) {
  return async function (
    fastify: FastifyInstance,
    _opts: FastifyPluginOptions,
  ) {
    const log = deps.logger.child({ routes: 'taps' });

    /**
     * GET /taps
     * List taps with filtering and pagination
     */
    fastify.get<{
      Querystring: {
        page?: number;
        limit?: number;
        search?: string;
        isVerified?: boolean;
        ownerId?: string;
      };
    }>('/taps', async (request, reply) => {
      try {
        const { page, limit, search, isVerified, ownerId } = request.query;

        const result = await deps.tapService.listTaps({
          page: page ? Number(page) : undefined,
          limit: limit ? Number(limit) : undefined,
          search,
          isVerified,
          ownerId,
        });

        if (!isOk(result)) {
          throw result.error;
        }

        return reply.send(result.value);
      } catch (error) {
        log.error({ error }, 'Failed to list taps');

        if (error instanceof AppError) {
          return reply.status(error.statusCode).send({
            error: {
              code: error.code || 'ERROR',
              message: error.message,
            },
          });
        }

        return reply.status(500).send({
          error: {
            code: 'INTERNAL_ERROR',
            message: 'Failed to list taps',
          },
        });
      }
    });

    /**
     * POST /taps
     * Create a new tap
     */
    fastify.post<{ Body: CreateTapRequest }>('/taps', async (request, reply) => {
      try {
        const auth = requireAuth(request);

        // Validate request body
        const validationResult = createTapRequestSchema.safeParse(request.body);
        if (!validationResult.success) {
          return reply.status(400).send({
            error: {
              code: 'VALIDATION_ERROR',
              message: 'Invalid request body',
              details: validationResult.error.issues,
            },
          });
        }

        const result = await deps.tapService.createTap(
          validationResult.data,
          auth.userId,
        );

        if (!isOk(result)) {
          if (result.error instanceof ValidationError) {
            return reply.status(400).send({
              error: {
                code: 'VALIDATION_ERROR',
                message: result.error.message,
              },
            });
          }
          throw result.error;
        }

        return reply.status(201).send({ data: result.value });
      } catch (error) {
        log.error({ error }, 'Failed to create tap');

        if (error instanceof AppError) {
          return reply.status(error.statusCode).send({
            error: {
              code: error.code || 'ERROR',
              message: error.message,
            },
          });
        }

        return reply.status(500).send({
          error: {
            code: 'INTERNAL_ERROR',
            message: 'Failed to create tap',
          },
        });
      }
    });

    /**
     * GET /taps/:tapId
     * Get details of a specific tap
     */
    fastify.get<{ Params: { tapId: string } }>(
      '/taps/:tapId',
      async (request, reply) => {
        try {
          const { tapId } = request.params;
          const auth = requireAuth(request);

          const result = await deps.tapService.getTap(tapId, auth.userId);

          if (!isOk(result)) {
            if (result.error instanceof NotFoundError) {
              return reply.status(404).send({
                error: {
                  code: 'TAP_NOT_FOUND',
                  message: result.error.message,
                },
              });
            }
            if (result.error instanceof ForbiddenError) {
              return reply.status(403).send({
                error: {
                  code: 'FORBIDDEN',
                  message: result.error.message,
                },
              });
            }
            throw result.error;
          }

          return reply.send({ data: result.value });
        } catch (error) {
          log.error({ error }, 'Failed to get tap');

          if (error instanceof AppError) {
            return reply.status(error.statusCode).send({
              error: {
                code: error.code || 'ERROR',
                message: error.message,
              },
            });
          }

          return reply.status(500).send({
            error: {
              code: 'INTERNAL_ERROR',
              message: 'Failed to get tap',
            },
          });
        }
      },
    );

    /**
     * PATCH /taps/:tapId
     * Update a tap
     */
    fastify.patch<{ Params: { tapId: string }; Body: UpdateTapRequest }>(
      '/taps/:tapId',
      async (request, reply) => {
        try {
          const { tapId } = request.params;
          const auth = requireAuth(request);

          // Validate request body
          const validationResult = updateTapRequestSchema.safeParse(request.body);
          if (!validationResult.success) {
            return reply.status(400).send({
              error: {
                code: 'VALIDATION_ERROR',
                message: 'Invalid request body',
                details: validationResult.error.issues,
              },
            });
          }

          const result = await deps.tapService.updateTap(
            tapId,
            validationResult.data,
            auth.userId,
          );

          if (!isOk(result)) {
            if (result.error instanceof NotFoundError) {
              return reply.status(404).send({
                error: {
                  code: 'TAP_NOT_FOUND',
                  message: result.error.message,
                },
              });
            }
            if (result.error instanceof ForbiddenError) {
              return reply.status(403).send({
                error: {
                  code: 'FORBIDDEN',
                  message: result.error.message,
                },
              });
            }
            throw result.error;
          }

          return reply.send({ data: result.value });
        } catch (error) {
          log.error({ error }, 'Failed to update tap');

          if (error instanceof AppError) {
            return reply.status(error.statusCode).send({
              error: {
                code: error.code || 'ERROR',
                message: error.message,
              },
            });
          }

          return reply.status(500).send({
            error: {
              code: 'INTERNAL_ERROR',
              message: 'Failed to update tap',
            },
          });
        }
      },
    );

    /**
     * DELETE /taps/:tapId
     * Delete a tap
     */
    fastify.delete<{ Params: { tapId: string } }>(
      '/taps/:tapId',
      async (request, reply) => {
        try {
          const { tapId } = request.params;
          const auth = requireAuth(request);

          const result = await deps.tapService.deleteTap(tapId, auth.userId);

          if (!isOk(result)) {
            if (result.error instanceof NotFoundError) {
              return reply.status(404).send({
                error: {
                  code: 'TAP_NOT_FOUND',
                  message: result.error.message,
                },
              });
            }
            if (result.error instanceof ForbiddenError) {
              return reply.status(403).send({
                error: {
                  code: 'FORBIDDEN',
                  message: result.error.message,
                },
              });
            }
            throw result.error;
          }

          return reply.status(204).send();
        } catch (error) {
          log.error({ error }, 'Failed to delete tap');

          if (error instanceof AppError) {
            return reply.status(error.statusCode).send({
              error: {
                code: error.code || 'ERROR',
                message: error.message,
              },
            });
          }

          return reply.status(500).send({
            error: {
              code: 'INTERNAL_ERROR',
              message: 'Failed to delete tap',
            },
          });
        }
      },
    );

    /**
     * GET /taps/:tapId/members
     * Get tap members
     */
    fastify.get<{ Params: { tapId: string } }>(
      '/taps/:tapId/members',
      async (request, reply) => {
        try {
          const { tapId } = request.params;
          const auth = requireAuth(request);

          const result = await deps.tapService.getTapMembers(tapId, auth.userId);

          if (!isOk(result)) {
            if (result.error instanceof NotFoundError) {
              return reply.status(404).send({
                error: {
                  code: 'TAP_NOT_FOUND',
                  message: result.error.message,
                },
              });
            }
            if (result.error instanceof ForbiddenError) {
              return reply.status(403).send({
                error: {
                  code: 'FORBIDDEN',
                  message: result.error.message,
                },
              });
            }
            throw result.error;
          }

          return reply.send({ data: result.value });
        } catch (error) {
          log.error({ error }, 'Failed to get tap members');

          if (error instanceof AppError) {
            return reply.status(error.statusCode).send({
              error: {
                code: error.code || 'ERROR',
                message: error.message,
              },
            });
          }

          return reply.status(500).send({
            error: {
              code: 'INTERNAL_ERROR',
              message: 'Failed to get tap members',
            },
          });
        }
      },
    );
  };
}
