import type { FastifyInstance, FastifyPluginOptions } from 'fastify';
import { healthRoutes, type HealthDependencies } from './health.js';
import { settingsRoutes, type SettingsRouteDependencies } from './settings.js';

export interface RouteDependencies extends HealthDependencies, SettingsRouteDependencies {}

export function registerRoutes(deps: RouteDependencies) {
  return async function (
    fastify: FastifyInstance,
    _opts: FastifyPluginOptions
  ) {
    await fastify.register(healthRoutes(deps));

    await fastify.register(
      async (apiInstance) => {
        await apiInstance.register(settingsRoutes(deps));
      },
      { prefix: '/api/v1' }
    );
  };
}

export { healthRoutes, type HealthDependencies } from './health.js';
export { settingsRoutes, type SettingsRouteDependencies } from './settings.js';
