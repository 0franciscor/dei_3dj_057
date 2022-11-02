import { Router } from 'express';
import { Container } from 'typedi';
import { celebrate, Joi } from 'celebrate';

import config from "../../../config";
import IPackagingController from '../../controllers/IControllers/IPackagingController';

const route = Router();

export default (app: Router) => {
   
    app.use('/packaging', route);

    const ctrl = Container.get(config.controllers.packaging.name) as IPackagingController;

    route.get('/id/:id', (req, res, next) =>  {
        req.body.packagingID = req.params.id;
        ctrl.getPackaging(req, res, next)
    });

    route.get('/all', (req, res, next) =>  ctrl.getAllPackagings(req, res, next));

    route.post('/',
        celebrate({
            body: Joi.object({
                packagingID: Joi.string().required(),
                truckID: Joi.string().required(),
                deliveryID: Joi.string().required(),
                xPosition: Joi.number().required(),
                yPosition: Joi.number().required(),
                zPosition: Joi.number().required()
            })
        }),
        
        (req, res, next) => {
            ctrl.createPackaging(req, res, next)}
    );


    route.put('/',
        celebrate({
            body: Joi.object({
                xPosition: Joi.number().required(),
                yPosition: Joi.number().required(),
                zPosition: Joi.number().required()
            })
        }),
        
        (req, res, next) => ctrl.updatePackaging(req, res, next)
    );

    route.delete('/:id', (req, res, next) => {
        req.body.packagingID = req.params.id;
        ctrl.deletePackaging(req, res, next)
    });


}