import  config  from "../../../config";
import IPathController from "../../controllers/IControllers/IPathController";
import { Router } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from "celebrate";

const route = Router();
const cors = require('cors');

export default(app: Router)=>{
  app.use('/path',route);
  app.use(cors({
      methods: ['GET','POST','DELETE','UPDATE','PUT','PATCH']
    }));

  const ctrl= Container.get(config.controllers.path.name) as IPathController;

  route.get('/id/:id',(req,res,next)=> {
    req.body.pathID = req.params.id
    ctrl.getPath(req,res,next);
  });
  
  route.get('/all',(req,res,next)=> ctrl.getAllPaths(req,res,next));
    
  route.post('/',
    celebrate({
      body: Joi.object({
          pathID:Joi.string().required(),
          startWHId: Joi.string().required(),
          destinationWHId: Joi.string().required(),
          pathDistance: Joi.number().required(),
          pathTravelTime: Joi.number().required(),
          wastedEnergy: Joi.number().required(),
          extraTravelTime: Joi.number().required()
        })
    }),
      (req,res,next)=> ctrl.createPath(req,res,next)
    );
    
  route.patch('/',
    celebrate({
      body: Joi.object({
        pathID:Joi.string(),
        startWHId: Joi.string(),
        destinationWHId: Joi.string(),
        pathDistance: Joi.number(),
        pathTravelTime: Joi.number(),
        wastedEnergy: Joi.number(),
        extraTravelTime: Joi.number()
      })
    }),(req,res,next)=> ctrl.updatePath(req,res,next));
  
  
    route.delete('/id/:id',(req,res,next)=>{ 
      req.body.id = req.params.id;
      ctrl.deletePath(req,res,next);
    });
    

}