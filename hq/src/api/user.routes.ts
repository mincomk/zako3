import type { FastifyInstance, FastifyPluginOptions } from 'fastify';
import type { Logger } from 'pino';
import { UserService } from '../services/user.service.js';
import { requireAuth } from './context.js';
import { isOk } from '../lib/result.js';
import {
  NotFoundError,
  UnauthorizedError,
  AppError,
} from '../lib/errors.js';

export interface UserRouteDependencies {
  userService: UserService;
  logger: Logger;
}

/**
 * User API routes
 */
export function userRoutes(deps: UserRouteDependencies) {
  return async function (
    fastify: FastifyInstance,
    _opts: FastifyPluginOptions,
  ) {
    const log = deps.logger.child({ routes: 'users' });

    /**
     * GET /users/me
     * Get current authenticated user's profile
     */
    fastify.get('/users/me', async (request, reply) => {
      try {
        const auth = requireAuth(request);
        const result = await deps.userService.getUser(auth.userId);

        if (!isOk(result)) {
          if (result.error instanceof NotFoundError) {
            return reply.status(404).send({
              error: {
                code: 'USER_NOT_FOUND',
                message: result.error.message,
              },
            });
          }
          throw result.error;
        }

        return reply.send({ data: result.value });
      } catch (error) {
        log.error({ error }, 'Failed to get current user');
        
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
            message: 'Failed to get user',
          },
        });
      }
    });

    /**
     * GET /users/:userId
     * Get public profile of a user
     */
    fastify.get<{ Params: { userId: string } }>(
      '/users/:userId',
      async (request, reply) => {
        try {
          const { userId } = request.params;
          const result = await deps.userService.getPublicProfile(userId);

          if (!isOk(result)) {
            if (result.error instanceof NotFoundError) {
              return reply.status(404).send({
                error: {
                  code: 'USER_NOT_FOUND',
                  message: result.error.message,
                },
              });
            }
            throw result.error;
          }

          return reply.send({ data: result.value });
        } catch (error) {
          log.error({ error }, 'Failed to get user');

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
              message: 'Failed to get user',
            },
          });
        }
      },
    );
  };
}
