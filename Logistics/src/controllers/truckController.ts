import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import ITruckController from './IControllers/ITruckController';
import ITruckService from '../services/IServices/ITruckService';
import { ITruckDTO } from '../dto/ITruckDTO';

import { Result } from '../core/logic/Result';

@Service()
export default class TruckController implements ITruckController {

    constructor(
        @Inject(config.services.truck.name) private truckService: ITruckService,
    ) { }

    public async getTruck(req: Request, res: Response, next: NextFunction){
        try {
            const truck = await this.truckService.getTruck(req.params.id);
            res.status(200).json(truck);
        } catch (e) {
            next(e);
        }
    }

    public async createTruck(req: Request, res: Response, next: NextFunction) {
        try {
            const truckOrError = await this.truckService.createTruck(req.body as ITruckDTO) as Result<ITruckDTO>;
            console.log("truckDTO");
            if (truckOrError.isFailure) {
                return res.status(402).send();
            }

            const truckDTO = truckOrError.getValue();
            
            return res.json( truckDTO ).status(201);


            } catch (e) {
            next(e);
        }
    }

    public async updateTruck(req: Request, res: Response, next: NextFunction) {
        try {
            const truckOrError = await this.truckService.updateTruck(req.body as ITruckDTO) as Result<ITruckDTO>;
            if (truckOrError.isFailure) {
                return res.status(402).send();
            }
            const truckDTO = truckOrError.getValue();
            return res.json( truckDTO ).status(201);
        } catch (e) {
            next(e);
        }
    }

    public async deleteTruck(req: Request, res: Response, next: NextFunction){
        try {
            const truckResult = await this.truckService.deleteTruck(req.params.id);
            res.status(200).json(truckResult);
        } catch (e) {
            next(e);
        }
    }

}