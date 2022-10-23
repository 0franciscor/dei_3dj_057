import { Router } from 'express';
import { Container } from 'typedi';


import config from "../../../config";
import ITruckController from '../../controllers/IControllers/ITruckController';

const route = Router();

export default (app: Router) => {
   
    app.use('/truck', route);

    const ctrl = Container.get(config.controllers.truck.name) as ITruckController;

    route.get('/', (req, res, next) =>  ctrl.getTruck(req, res, next));

    route.get('/all', (req, res, next) =>  ctrl.getAllTrucks(req, res, next));


    route.post('/', (req, res, next) => ctrl.createTruck(req, res, next));


    route.put('/', (req, res, next) => {     
        ctrl.updateTruck(req, res, next);
    });

    route.delete('/', (req, res, next) => {
        ctrl.deleteTruck(req, res, next);
    });


}