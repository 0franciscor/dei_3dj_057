import { Request, Response, NextFunction } from 'express';
import { ParamsDictionary } from 'express-serve-static-core';
import { ParsedQs } from 'qs';
import { Inject, Service } from 'typedi';
import config from "../../config";
import { Result } from '../core/logic/Result';
import { IRouteDTO } from '../dto/IRouteDTO';
import IRouteService from '../services/IServices/IRouteService';
import IRouteController from './IControllers/IRouteController';

@Service()
export default class RouteController implements IRouteController{

    constructor(
        @Inject(config.services.routes.name) private routeService: IRouteService,
    ) { }

    public async getRoute(req: Request, res: Response, next: NextFunction ) {
        try{
            const route = await this.routeService.getRoute(req.body.routeID);
            if(route.isFailure){
                res.status(404);
                return res.send("Route not found");
            }

            res.status(200);
            return res.json(route.getValue());
        }catch(e){
            next(e);
        }
    }

    public async getAllRoutes(req: Request, res: Response ,next: NextFunction) {
        try {
            const routes = await this.routeService.getAllRoutes();
            res.status(200);
            return res.json(routes.getValue());
        } catch(e) {
            next(e);
        }
        

    }

    public async createRoute(req: Request, res: Response, next: NextFunction) {
        try{
            const routeOrError = await this.routeService.createRoute(req.body as IRouteDTO);

            if(routeOrError.isFailure) {
                res.status(409);
                return res.send("Route already exists");
            }

            const routeDTO = routeOrError.getValue();
            res.status(201);
            return res.json( routeDTO ); 
        } catch(e){
            next(e);
        }
    }

    public async updateRoute(req: Request, res: Response, next: NextFunction) {
        try {
            const routeOrError = await this.routeService.updateRoute(req.body as IRouteDTO) as Result<IRouteDTO>;
            if(routeOrError.isFailure) {
                res.status(404);
                return res.send("Route not found");
            }
            const routeDTO = routeOrError.getValue();
            res.status(200);
            return res.json(routeDTO);
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

    
}