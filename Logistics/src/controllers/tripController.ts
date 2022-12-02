import { Request, Response, NextFunction } from 'express';
import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from "../../config";

import { Result } from '../core/logic/Result';
import { ITripDTO } from '../dto/ITripDTO';
import IPackagingService from '../services/IServices/IPackagingService';
import IPathService from '../services/IServices/IPathService';
import ITripService from '../services/IServices/ITripService';
import ITruckService from '../services/IServices/ITruckService';
import ITripController from './IControllers/ITripController';
import fetch from 'fetch';
import { request } from 'http';
import { StartWHId } from '../domain/path/StartWHId';
import { PathID } from '../domain/path/PathID';

const http = require('https');


@Service()
export default class TripController implements ITripController{

    constructor(
        @Inject(config.services.trip.name) private tripService: ITripService,
        @Inject(config.services.truck.name) private truckService: ITruckService,
        @Inject(config.services.path.name) private pathService: IPathService,
        @Inject(config.services.packaging.name) private packagingService: IPackagingService
    ) { }

    public async getTrip(req: Request, res: Response, next: NextFunction ) {
        try{
            const trip = await this.tripService.getTrip(req.body.tripID);
            if(trip.isFailure){
                return res.status(404).send("Trip not found");
            }

            res.status(200).json(trip);
        }catch(e){
            next(e);
        }
    }

    public async getAllTrips(req: Request, res: Response ,next: NextFunction) {
        try {
            const trips = await this.tripService.getAllTrips();
            if(trips.isFailure)
            return res.status(404).send("Trip not found");

            res.status(200).json(trips);
        } catch(e) {
            next(e);
        }
        

    }

    public async createTrip(req: Request, res: Response, next: NextFunction) {
      try{
        if(req.body.tripID == null)
            return res.status(400).send("TripID is required");
            
        

            const pathOrError = await this.pathService.exist(req.body.pathID);

            const truckOrError = await this.truckService.exist(req.body.truckID);
            if(truckOrError.getValue() == false) {
                return res.status(404).send("Truck not found");
            } 

            
            const packagingOrError = await this.packagingService.exist(req.body.packagingID);
            if(packagingOrError.getValue() == false) {
                return res.status(404).send("Packaging not found");
            }


            const tripOrError = await this.tripService.createTrip(req.body as ITripDTO) as Result<ITripDTO>;
            if(tripOrError.isFailure){
                return res.status(409).send("Trip already exists");
            }

            const tripDTO = tripOrError.getValue();

            return res.status(201).json(tripDTO);


        }catch(e) {
            next(e);
        }
    }

    public async updateTrip(req: Request, res: Response, next: NextFunction) {
        try {
            if(req.body.tripID == null)
                return res.status(400).send("TripID is required");
                const tripOrError = await this.tripService.updateTrip(req.body as ITripDTO);
                    if(tripOrError.isFailure) {
                        if(tripOrError.error == "TripID cannot be changed")
                            return res.status(400).send(tripOrError.error);
                        if(tripOrError.error == "PathID cannot be changed")
                            return res.status(400).send(tripOrError.error);
                        if(tripOrError.error == "TruckID cannot be changed ")
                            return res.status(400).send(tripOrError.error);
                        if(tripOrError.error == "PackagingID cannot be changed")
                            return res.status(400).send(tripOrError.error);
                    }
                    const tripDTO = tripOrError.getValue();

                    return res.status(200).json(tripDTO);
                }catch(e) {
                    next(e);
                }
            }
    

    public async deleteTrip(req: Request, res: Response, next: NextFunction) {
        try {
            const tripResult = await this.tripService.deleteTrip(req.body.tripID);
            if(tripResult.isFailure){
                res.status(404);
                return res.send("Trip not found");
            }

            res.status(200);
            return res.json(tripResult.getValue());
        }catch(e){
            next(e);
        }       
    }

    private async fetch(address : string){
        const httpAgent = new http.Agent({rejectUnauthorized: false});
        return await fetch(address, {
            method: 'GET',
            agent: httpAgent
        });
    }

    
}

