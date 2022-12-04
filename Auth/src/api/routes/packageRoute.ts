import { Router } from "express";
import Container from "typedi";
import config from "../../../config";
import IPackageController from "../../controllers/IControllers/IPackageController";

const route = Router();


export default (app: Router)=> {
    app.use('/packaging',route);

    const ctrl = Container.get(config.controllers.package.name) as IPackageController;

    route.get('/all',(req,res,next)=>{
        ctrl.getAllPackage(req,res,next)

    });

    route.post('/',(req,res,next) => ctrl.createPackage(req,res,next))

}
