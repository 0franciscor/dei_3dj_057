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

  const truckController = {
    name: config.controllers.truck.name,
    path: config.controllers.truck.path
  }

  const pathController={
    name: config.controllers.path.name,
    path:config.controllers.path.path,
  };

  const truckRepo = {
    name: config.repos.truck.name,
    path: config.repos.truck.path
  }

  const pathRepo={
    name: config.repos.path.name,
    path: config.repos.path.path
  };


  const truckService = {
    name: config.services.truck.name,
    path: config.services.truck.path
  }

  const pathService={
    name: config.services.path.name,
    path: config.services.path.path
  };

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
        truckSchema,
        pathSchema
    ],
    controllers: [
        truckController,
        pathController
    ],
    repos: [
        truckRepo,
        pathRepo
    ],
    services: [
        truckService,
        pathService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({ app: expressApp });
  Logger.info('✌️ Express loaded');
};
