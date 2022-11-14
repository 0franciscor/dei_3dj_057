import { Router, Request, Response, NextFunction } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IUserController from '../../controllers/IControllers/IUserController';

const route = Router();

export default (app: Router) => {
    app.use('/SPA', route);

    const ctrl = Container.get(config.controllers.user.name) as IUserController;

    route.get ('/id/:id',(req,res,next)=>
    {
        req.body.userId = req.params.id;
        ctrl.getUser(req,res,next)
    });

    route.get('/all', (req,res,next)=>
    {
        ctrl.getAllUsers(req,res,next)
    });

    route.post('/',
        celebrate({
            body: Joi.object({
                userId: Joi.string().required(),
                firstName: Joi.string().required(),
                lastName: Joi.string().required(),
                email: Joi.string().required(), 
                password: Joi.string().required(),
                role: Joi.string().required()
            })
        }),
        
        (req,res,next) => ctrl.createUser(req,res,next)    
    );
    
    route.patch('/',
        celebrate({
            body: Joi.object({
                userId: Joi.string().required(),
                firstName: Joi.string(),
                lastName: Joi.string(),
                email: Joi.string(),
                password: Joi.string().required(),
                role: Joi.string()
            })
        }),
        (req,res,next) => ctrl.updateUser(req,res,next) 
    );

    route.delete('/id/:id',(req,res,next)=>{
        req.body.userId = req.params.id;
        ctrl.deleteUser(req,res,next)
    });
 
};
