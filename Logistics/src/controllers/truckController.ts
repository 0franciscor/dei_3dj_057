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

    public async getTruck(req: Request, res: Response, next: NextFunction): Promise<void> {
        try {
            const truck = await this.truckService.getTruck(req.params.id);
            res.status(200).json(truck);
        } catch (e) {
            next(e);
        }
    }

    public async createTruck(req: Request, res: Response, next: NextFunction): Promise<void> {
        try {
            const truckDTO: ITruckDTO = req.body;
            const truckResult = await this.truckService.createTruck(truckDTO);
            res.status(200).json(truckResult);
        } catch (e) {
            next(e);
        }
    }

    public async updateTruck(req: Request, res: Response, next: NextFunction): Promise<void> {
        try {
            const truckDTO: ITruckDTO = req.body;
            const truckResult = await this.truckService.updateTruck(truckDTO);
            res.status(200).json(truckResult);
        } catch (e) {
            next(e);
        }
    }

    public async deleteTruck(req: Request, res: Response, next: NextFunction): Promise<void> {
        try {
            const truckResult = await this.truckService.deleteTruck(req.params.id);
            res.status(200).json(truckResult);
        } catch (e) {
            next(e);
        }
    }

}