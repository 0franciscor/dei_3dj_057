import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
//import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IDeliveryController from '../../controllers/IControllers/IDeliveryController';

const route = Router();

export default (app: Router) => {
    app.use('/delivery', route);

    const ctrl = Container.get(config.controllers.delivery.name) as IDeliveryController;


    route.get('/all', (req,res,next)=>
    {
        ctrl.getAllDeliveries(req,res,next)
    });

    route.get('/:id', (req,res,next)=>
    {
        ctrl.getDelivery(req,res,next)
    });

    route.post('/create', (req,res,next)=>
    {
        ctrl.createDelivery(req,res,next)
    });

    route.patch('/update', (req,res,next)=>
    {
        ctrl.updateDelivery(req,res,next)
    });
};