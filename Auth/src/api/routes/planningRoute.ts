import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IPlanningController from '../../controllers/IControllers/IPlanningControler';

const route = Router();

export default (app: Router) => {
    app.use('/planning', route);

    const ctrl = Container.get(config.controllers.path.name) as IPlanningController;


    route.post('/bestPath',
        (req,res,next)=> ctrl.findAllBestPath(req,res,next)
    );

    route.post('/heuristicMass',
        (req,res,next)=>ctrl.heuristicMass(req,res,next)
    
    );

    route.post('/heuristicClosestWarehouse',
        (req,res,next)=>ctrl.heuristicClosestWarehouse(req,res,next)
    );

    route.post('/heuristicMassAndDistance',
        (req,res,next)=>ctrl.heuristicMassAndDistance(req,res,next)
    );
    
 
};