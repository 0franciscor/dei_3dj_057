import { celebrate, Joi } from "celebrate";
import { Router } from "express";
import Container from "typedi";
import config from "../../../config";
import ITripController from "../../controllers/IControllers/ITripController";


const routeL = Router();

export default(app: Router) => {

    app.use('/trip',routeL);

    const ctrl = Container.get(config.controllers.trip.name) as ITripController;

    routeL.get('/id/:id',(req,res,next) => {
        req.body.tripID = req.params.id;
        ctrl.getTrip(req,res,next)
    });

    routeL.get('/all',(req,res,next)=> {
        ctrl.getAllTrips(req,res,next)
    });

    routeL.post('/',
    celebrate({
        body: Joi.object({
            tripID: Joi.string().required(),
            date: Joi.string().required(),
            pathIDlist: Joi.array().required(),
            truckID: Joi.string().required(),
            deliveryIDlist: Joi.array().required()
        })
    }),
    
    (req,res,next) => ctrl.createTrip(req, res, next)
    
    );
   
    routeL.patch( '/',
    celebrate({
        body: Joi.object({
            tripID: Joi.string().required(),
            date: Joi.string(),
            pathIDlist: Joi.array(),
            truckID: Joi.string(),
            deliveryIDlist: Joi.array()
        })
    }),

    (req,res,next) => ctrl.updateTrip(req,res,next)

    );

    routeL.delete('/id/:id', (req,res,next) => {
        req.body.tripID = req.params.id;
        ctrl.deleteTrip(req,res,next)
    });

}