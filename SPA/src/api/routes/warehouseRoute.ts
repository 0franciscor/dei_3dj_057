import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IWarehouseController from '../../controllers/IControllers/IWarehouseController';

const route = Router();

export default (app: Router) => {
    app.use('/warehouse', route);

    const ctrl = Container.get(config.controllers.warehouse.name) as IWarehouseController;


    route.get('/all', (req,res,next)=>
    {
        ctrl.getAllWarehouse(req,res,next)
    });

    
 
};
