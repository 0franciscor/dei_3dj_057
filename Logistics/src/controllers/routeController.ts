import { Request, Response, NextFunction } from 'express';
import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from "../../config";
import routeRoute from '../api/routes/routeRoute';
import { Result } from '../core/logic/Result';
import { IRouteDTO } from '../dto/IRouteDTO';
import IPackagingService from '../services/IServices/IPackagingService';
import IPathService from '../services/IServices/IPathService';
import IRouteService from '../services/IServices/IRouteService';
import ITruckService from '../services/IServices/ITruckService';
import IRouteController from './IControllers/IRouteController';
import fetch from 'fetch';
import { request } from 'http';
import { StartWHId } from '../domain/path/StartWHId';
import { PathID } from '../domain/path/PathID';

const http = require('https');


@Service()
export default class RouteController implements IRouteController{

    constructor(
        @Inject(config.services.routes.name) private routeService: IRouteService,
        @Inject(config.services.truck.name) private truckService: ITruckService,
        @Inject(config.services.path.name) private pathService: IPathService,
        @Inject(config.services.packaging.name) private packagingService: IPackagingService
    ) { }

    public async getRoute(req: Request, res: Response, next: NextFunction ) {
        try{
            const route = await this.routeService.getRoute(req.body.routeID);
            if(route.isFailure){
                return res.status(404).send("Route not found");
            }

            res.status(200).json(route);
        }catch(e){
            next(e);
        }
    }

    public async getAllRoutes(req: Request, res: Response ,next: NextFunction) {
        try {
            const routes = await this.routeService.getAllRoutes();
            if(routes.isFailure)
            return res.status(404).send("Route not found");

            res.status(200).json(routes);
        } catch(e) {
            next(e);
        }
        

    }

    public async createRoute(req: Request, res: Response, next: NextFunction) {
      try{
        if(req.body.routeID == null)
            return res.status(400).send("RouteID is required");
            
        

            const pathOrError = await this.pathService.exist(req.body.pathID);

            const truckOrError = await this.truckService.exist(req.body.truckID);
            if(truckOrError.getValue() == false) {
                return res.status(404).send("Truck not found");
            } 

            
            const packagingOrError = await this.packagingService.exist(req.body.packagingID);
            if(packagingOrError.getValue() == false) {
                return res.status(404).send("Packaging not found");
            }


            const routeOrError = await this.routeService.createRoute(req.body as IRouteDTO) as Result<IRouteDTO>;
            if(routeOrError.isFailure){
                return res.status(409).send("Route already exists");
            }

            const routeDTO = routeOrError.getValue();

            return res.status(201).json(routeDTO);


        }catch(e) {
            next(e);
        }
    }

    public async updateRoute(req: Request, res: Response, next: NextFunction) {
        try {
            if(req.body.routeID == null)
                return res.status(400).send("RouteID is required");
                const routeOrError = await this.routeService.updateRoute(req.body as IRouteDTO);
                    if(routeOrError.isFailure) {
                        if(routeOrError.error == "RouteID cannot be changed")
                            return res.status(400).send(routeOrError.error);
                        if(routeOrError.error == "PathID cannot be changed")
                            return res.status(400).send(routeOrError.error);
                        if(routeOrError.error == "TruckID cannot be changed ")
                            return res.status(400).send(routeOrError.error);
                        if(routeOrError.error == "PackagingID cannot be changed")
                            return res.status(400).send(routeOrError.error);
                    }
                    const routeDTO = routeOrError.getValue();

                    return res.status(200).json(routeDTO);
                }catch(e) {
                    next(e);
                }
            }
    

    public async deleteRoute(req: Request, res: Response, next: NextFunction) {
        try {
            const routeResult = await this.routeService.deleteRoute(req.body.routeID);
            if(routeResult.isFailure){
                res.status(404);
                return res.send("Route not found");
            }

            res.status(200);
            return res.json(routeResult.getValue());
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

