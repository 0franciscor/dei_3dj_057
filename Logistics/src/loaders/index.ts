import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose’;
import config from ‘../../config.js';
export default async ({ expressApp }) => {
    const mongoConnection = await mongooseLoader();
    const roleModel = {
        name: 'roleModel',
        model: require('../persistence/role').default,
    };
    const roleController = {
    name: config.controller.role.name,
    path: config.controller.role.path
    }
    const roleRepo = {
    name: config.repos.role.name,
    path: config.repos.role.path
    }
    const roleService = {
    name: config.services.role.name,
    path: config.services.role.path
    }
    await dependencyInjectorLoader({
    mongoConnection,
    schemas: [roleSchema],
    controllers: [roleController],
    repos: [roleRepo],
    services: [roleService]
    });
await expressLoader({ app: expressApp });
}