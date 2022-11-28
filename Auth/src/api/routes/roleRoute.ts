import  config  from "../../../config";
import { Router } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from 'celebrate';
import IRoleController from "../../controllers/IControllers/IRoleController";


const route= Router();

export default (app: Router)=>{
    app.use('/role',route);

    const ctrl = Container.get(config.controllers.role.name) as IRoleController;

    route.get('/id/:id',(req,res,next)=>{
        req.body.roleId = req.params.id;
        ctrl.getRole(req,res,next)
    });

    route.get('/all',(req,res,next)=>{
        ctrl.getAllRoles(req,res,next);
    });

    route.post('/',
        celebrate({
            body: Joi.object({
                roleId: Joi.string().required(),
                name: Joi.string().required()
            })
        }),
        (req,res,next)=> ctrl.createRole(req,res,next)       	
    );

    route.patch('/',
        celebrate({
            body: Joi.object({
                roleId: Joi.string().required(),
                name: Joi.string()
            })
        }),
        (req,res,next) => ctrl.updateRole(req,res,next) 
    );

    route.delete('/id/:id',(req,res,next)=>{
        req.body.roleId = req.params.id;
        ctrl.deleteRole(req,res,next)
    });
 

}