import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';

import config from '../../config.js';

export default async ({ expressApp }) => {
    const mongoConnection = await mongooseLoader();
    Logger.info('DB loaded and connected!');

    const truckSchema = {
        name: 'truckSchema',
        schema: '../persistence/schemas/truckSchema',
    };

    const truckController = {
        name: config.controllers.truck.name,
        path: config.controllers.truck.path
    };

    const truckRepo = {
        name: config.repos.truck.name,
        path: config.repos.truck.path
    };

    const truckService = {
        name: config.services.truck.name,
        path: config.services.truck.path
    };

    await dependencyInjectorLoader({
        mongoConnection,
        schemas: [truckSchema],
        controllers: [truckController],
        repos: [truckRepo],
        services: [truckService]
    });
    Logger.info('Dependency Injector loaded');
    await expressLoader({ app: expressApp });
    Logger.info('Express loaded');
}