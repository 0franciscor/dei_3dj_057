import { Container } from 'typedi';
import LoggerInstance from './logger';

export default ({ mongoConnection, schemas, controllers, repos, services}: {
      mongoConnection: any;
      schemas: { name: string; schema: any }[];
      controllers: { name: string; path: string }[];
      repos: { name: string; path: string }[];
      services: { name: string; path: string }[]; }) => {
    
  try {
    Container.set('logger', LoggerInstance);

    schemas.forEach((m) => {
      let schema = require(m.schema).default;
      Container.set(m.name, schema);
    });
    repos.forEach((r) => {
      let repoClass = require(r.path).default;
      console.log(r.path);
      console.log(repoClass);
      let repoInstance = Container.get(repoClass);
      Container.set(r.name, repoInstance);
    });
    services.forEach((s) => {
      let serviceClass = require(s.path).default;
      let serviceInstance = Container.get(serviceClass)
      Container.set(s.name, serviceInstance);
    });
    controllers.forEach((c) => {
      let controllerClass = require(c.path).default;
      let controllerInstance = Container.get(controllerClass);
      Container.set(c.name, controllerInstance);
    });
    
  } catch (e) {
    LoggerInstance.error('Error on dependency injector loader: %o', e);
    throw e;
  }
};
