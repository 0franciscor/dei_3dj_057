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

    route.get('/allCities', (req,res,next)=>
    {
        ctrl.getAllCities(req,res,next)
    });

    route.post('/create', (req,res,next)=>
    {
        ctrl.createWarehouse(req,res,next)
    });

    route.post('/createProlog', (req,res,next)=>
    {
        ctrl.createWarehouseProlog(req,res,next)
    });

    route.put('/update', (req,res,next)=>
    {
        ctrl.editWarehouse(req,res,next)
    });

    route.put('/updateProlog', (req,res,next)=>
    {
        ctrl.editWarehouseProlog(req,res,next)
    });

    route.get('/:id',(req,res,next)=>{
        ctrl.getWarehouse(req,res,next)
    });

    
    
 
};