import  config  from "../../../config";
import IPathController from "../../controllers/IControllers/IPathController";
import { Router } from 'express';
import { Container } from 'typedi';

const route = Router();

export default(app: Router)=>{
    app.use('/path',route);

    const ctrl= Container.get(config.controllers.path.name) as IPathController;

    route.get('/',(req,res,next)=> ctrl.getPath(req,res,next));
    route.get('/',(req,res,next)=> ctrl.getAllPaths(req,res,next));
    route.get('/',(req,res,next)=> ctrl.createPath(req,res,next));
    route.get('/',(req,res,next)=> ctrl.updatePath(req,res,next));
    route.get('/',(req,res,next)=> ctrl.deletePath(req,res,next));

}