import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config';

export default async ({ expressApp }) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');


  const truckSchema = {
    // compare with the approach followed in repos and services
    name: 'truckSchema',
    schema: '../persistence/schemas/truckSchema',
  };

  const pathSchema={
    name: 'pathSchema',
    schema: '../persistence/schemas/pathSchema',
  };

  const packagingSchema={
    name: 'packagingSchema',
    schema: '../persistence/schemas/packagingSchema',
  };

  const routeSchema={
    name: 'routeSchema',
    schema: '../persistence/schemas/routeSchema',
  };

  const truckController = {
    name: config.controllers.truck.name,
    path: config.controllers.truck.path
  };

  const routeController = {
    name: config.controllers.route.name,
    path: config.controllers.route.path
  };

  const pathController={
    name: config.controllers.path.name,
    path: config.controllers.path.path,
  };

  const packagingController={
    name: config.controllers.packaging.name,
    path: config.controllers.packaging.path,
  };

  const truckRepo = {
    name: config.repos.truck.name,
    path: config.repos.truck.path
  };

  const routeRepo = {
    name: config.repos.route.name,
    path: config.repos.route.path
  };

  const pathRepo={
    name: config.repos.path.name,
    path: config.repos.path.path
  };

  const packagingRepo={
    name: config.repos.packaging.name,
    path: config.repos.packaging.path
  };


  const truckService = {
    name: config.services.truck.name,
    path: config.services.truck.path
  };

  const routeService = {
    name: config.services.routes.name,
    path: config.services.routes.path
  };

  const pathService={
    name: config.services.path.name,
    path: config.services.path.path
  };

  const packagingService={
    name: config.services.packaging.name,
    path: config.services.packaging.path
  }

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
        truckSchema,
        pathSchema,
        packagingSchema,
        routeSchema
    ],
    controllers: [
        truckController,
        pathController,
        packagingController,
        routeController
    ],
    repos: [
        truckRepo,
        pathRepo,
        packagingRepo,
        routeRepo
    ],
    services: [
        truckService,
        pathService,
        packagingService,
        routeService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
