import { celebrate, Joi } from 'celebrate';
import { Router } from 'express';
import { Container } from 'typedi';

import config from "../../../config";
import IUserController from '../../controllers/IControllers/IUserController';

const route = Router();

export default (app: Router) => {
    app.use('/user', route);

    const ctrl = Container.get(config.controllers.user.name) as IUserController;

    //login
    route.post('/loginWithGoogle', (req,res,next) => ctrl.loginWithGoogle(req,res,next));
    route.post('/login', (req,res,next) => ctrl.login(req,res,next));

    route.get ('/id',(req,res,next)=>
    {
        ctrl.getUser(req,res,next)
    });

    route.get('/all', (req,res,next)=>
    {
        ctrl.getAllUsers(req,res,next)
    });

    route.post('/',
        celebrate({
            body: Joi.object({
                firstName: Joi.string().required(),
                lastName: Joi.string().required(),
                email: Joi.string().required(), 
                phoneNumber: Joi.string(),
                password: Joi.string().required(),
                role: Joi.string().required()
            })
        }),
        (req,res,next) => ctrl.createUser(req,res,next)    
    );
    
    route.patch('/',
        celebrate({
            body: Joi.object({
                id: Joi.string().required(),
                firstName: Joi.string(),
                lastName: Joi.string(),
                email: Joi.string(),
                phoneNumber: Joi.string(),
                password: Joi.string(),
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
