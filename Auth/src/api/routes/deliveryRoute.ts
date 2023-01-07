import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
//import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IDeliveryController from '../../controllers/IControllers/IDeliveryController';

const route = Router();

export default (app: Router) => {
    app.use('/delivery', route);

    const ctrl = Container.get(config.controllers.delivery.name) as IDeliveryController;

    // ################# .NET WEB API RELATED #################

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

    route.delete('/delete/:id', (req,res,next)=>
    {
        ctrl.deleteDelivery(req,res,next)
    });





    // ################# PROLOG RELATED #################

    route.get('/allProlog', (req,res,next)=>
    {
        ctrl.getAllDeliveriesProlog(req,res,next)
    });

    route.post('/createProlog', (req,res,next)=>
    {
        ctrl.createDeliveryProlog(req,res,next)
    });

    route.put('/updateProlog', (req,res,next)=>
    {
        ctrl.updateDeliveryProlog(req,res,next)
    });

    route.delete('/deleteProlog', (req,res,next)=>
    {
        ctrl.deleteDeliveryProlog(req,res,next)
    });
    


    
    // ################# OTHER #################

    route.post("/getDeliveryDestination",(req,res,next)=>{
        ctrl.getDeliveryDestination(req,res,next)
    })
};