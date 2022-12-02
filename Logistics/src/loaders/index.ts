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

  const tripSchema={
    name: 'tripSchema',
    schema: '../persistence/schemas/tripSchema',
  };

  const truckController = {
    name: config.controllers.truck.name,
    path: config.controllers.truck.path
  };

  const tripController = {
    name: config.controllers.trip.name,
    path: config.controllers.trip.path
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

  const tripRepo = {
    name: config.repos.trip.name,
    path: config.repos.trip.path
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

  const tripService = {
    name: config.services.trip.name,
    path: config.services.trip.path
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
        tripSchema
    ],
    controllers: [
        truckController,
        pathController,
        packagingController,
        tripController
    ],
    repos: [
        truckRepo,
        pathRepo,
        packagingRepo,
        tripRepo
    ],
    services: [
        truckService,
        pathService,
        packagingService,
        tripService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
