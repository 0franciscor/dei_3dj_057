import { Router } from 'express';
import { Container } from 'typedi';
import config from "../../../config";
import ITripController from "../../controllers/IControllers/ITripController";


const route= Router();

export default (app: Router)=>{
    app.use('/trip',route);

    const ctrl = Container.get(config.controllers.trip.name) as ITripController;

    

    route.post('/',
        
        (req,res,next)=> ctrl.createTrip(req,res,next)       	
    );

    
 

}