import  config  from "../../../config";
import { Router } from 'express';
import { Container } from 'typedi';
import ITruckController from "../../controllers/IControllers/ITruckController";


const route= Router();

export default (app: Router)=>{
    app.use('/truck',route);

    const ctrl = Container.get(config.controllers.truck.name) as ITruckController;

    route.get('/id/:id',(req,res,next)=>{
        req.body.truckId = req.params.id;
        ctrl.getTruck(req,res,next)
    });

    route.get('/all',(req,res,next)=>{
        ctrl.getAllTruck(req,res,next)
    });

    route.post('/',
        (req,res,next) => ctrl.createTruck(req,res,next)
    );

    route.patch('/',
        (req,res,next) => ctrl.editTruck(req,res,next)
    );

    route.delete('/id/:id',
        (req,res,next) => ctrl.softDeleteTruck(req,res,next)
    );
    route.delete('/hard/id/:id',
        (req,res,next) => ctrl.hardDeleteTruck(req,res,next)
    );

    route.post('/prolog',
        (req,res,next) => ctrl.createTruckProlog(req,res,next)
    );

    route.patch('/prolog',
        (req,res,next) => ctrl.editTruckProlog(req,res,next)
    );

    route.delete('/idProlog/:id',
        (req,res,next) => ctrl.deleteTruckProlog(req,res,next)
    );
 

}