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
            
            const truck = await this.truckService.getTruck(req.body.truckID);
            if (truck.isFailure)
                return res.status(404).send("Truck not found");
                
            
            res.status(200).json(truck);
        } catch (e) {
            next(e);
        }
    }

    public async getAllTrucks(req: Request, res: Response, next: NextFunction){
        try {
            
            const trucks = await this.truckService.getAllTrucks();
            if (trucks.isFailure)
                return res.status(404).send("Trucks not found");

            res.status(200).json(trucks);
        } catch (e) {
            next(e);
        }
    }

    public async createTruck(req: Request, res: Response, next: NextFunction) {
        try {
            const truckOrError = await this.truckService.createTruck(req.body as ITruckDTO) as Result<ITruckDTO>;
            if (truckOrError.isFailure) {
                return res.status(403).send("Truck already exists");
            }

            const truckDTO = truckOrError.getValue();
            
            return res.status(201).json( truckDTO );


            } catch (e) {
            next(e);
        }
    }

    public async updateTruck(req: Request, res: Response, next: NextFunction) {
        try {
            const truckOrError = await this.truckService.updateTruck(req.body as ITruckDTO) as Result<ITruckDTO>;
            if (truckOrError.isFailure) {
                return res.status(403).send("Truck not found");
            }
            const truckDTO = truckOrError.getValue();
            return res.status(201).json( truckDTO );
        } catch (e) {
            next(e);
        }
    }

    public async deleteTruck(req: Request, res: Response, next: NextFunction){
        try {

            const truckResult = await this.truckService.deleteTruck(req.body.truckID);
            if(truckResult.isFailure)
                return res.status(403).send("Truck not found");
            res.status(200).json(truckResult);
        } catch (e) {
            next(e);
        }
    }

}