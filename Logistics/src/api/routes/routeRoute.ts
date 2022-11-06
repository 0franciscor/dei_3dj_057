import { celebrate, Joi } from "celebrate";
import { Router } from "express";
import Container from "typedi";
import config from "../../../config";
import IRouteController from "../../controllers/IControllers/IRouteController";


const routeL = Router();

export default(app: Router) => {

    app.use('/route',routeL);

    const ctrl = Container.get(config.controllers.route.name) as IRouteController;

    routeL.get('/id/:id',(req,res,next) => {
        req.body.routeID = req.params.id;
        ctrl.getRoute(req,res,next)
    });

    routeL.get('/all',(req,res,next)=> {
        ctrl.getAllRoutes(req,res,next)
    });

    routeL.post('/',
    celebrate({
        body: Joi.object({
            routeID: Joi.string().required(),
            date: Joi.string().required(),
            warehouses: Joi.string().required()
        })
    }),
    
    (req,res,next) => ctrl.createRoute(req, res, next)
    
    );
   
    routeL.patch( '/',
    celebrate({
        body: Joi.object({
            routeID: Joi.string().required(),
            date: Joi.string().required(),
            warehouse: Joi.string().required()
        })
    }),

    (req,res,next) => ctrl.updateRoute(req,res,next)

    );

    routeL.delete('/id/:id', (req,res,next) => {
        req.body.routeID = req.params.id;
        ctrl.deleteRoute(req,res,next)
    });

}