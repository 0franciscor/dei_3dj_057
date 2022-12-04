import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IPathController from '../../controllers/IControllers/IPathController';

const route = Router();

export default (app: Router) => {
    app.use('/path', route);

    const ctrl = Container.get(config.controllers.path.name) as IPathController;


    route.get('/all/:startWHId/:destinationWHId',(req,res,next)=> ctrl.getAllPaths(req,res,next));

    route.post('/',(req,res,next)=>ctrl.createPath(req,res,next)
    
    );

    route.post('/prolog',(req,res,next)=>ctrl.createPathProlog(req,res,next)
    
    );
    
 
};