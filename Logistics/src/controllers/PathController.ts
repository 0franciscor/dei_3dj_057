import config from "../../config";
import { Inject, Service } from "typedi";
import  IPathController  from "../controllers/IControllers/IPathController"
import IPathService from "../services/IServices/IPathService";
import { Request, Response, NextFunction } from "express";
import { IPathDTO } from "../dto/IPathDTO";
import { Result } from "../core/logic/Result";
import fetch from 'node-fetch'
const http = require ('https');




@Service()
export default class PathController implements IPathController{
    constructor(
        @Inject (config.services.path.name) private pathService: IPathService,) {}
    
    public async getPath(req: Request, res: Response, next: NextFunction) {
        try {
            const path = await this.pathService.getPath(req.body.pathID);
            if (path.isFailure)
                return res.status(404).send("Path not found")
            res.status(200).json(path);
        } catch (e) {
            next(e);
        }
    }

    public async getAllPaths(req: Request, res: Response, next: NextFunction) {
        try {
            const paths= await this.pathService.getAllPath(req.body);
            if(paths.isFailure)
                return res.status(404).send("No paths found");
            res.status(200).json(paths);
        } catch (e) {
            next(e);
        }
    }

    public async createPath(req: Request, res: Response, next: NextFunction) {
        try{
        
            const httpAgent = new http.Agent({rejectUnauthorized: false});
            const address_start = 'https://localhost:5001/api/warehouses/Exists/' + req.body.startWHId;

            const response_start = await fetch(address_start,{
                method: 'GET',
                agent : httpAgent
            });

            if(response_start.status==404)
                return res.status(404).send("Start Warehouse not found");
            
            const address_destination ='https://localhost:5001/api/warehouses/Exists/' + req.body.destinationWHId;
           const response_destination = await fetch(address_destination,{
                method : 'GET',
                agent: httpAgent
           });

           if (response_destination.status == 404)
                return res.status(404).send("Destination warehouse not found");
            
            const pathOrError = await this.pathService.createPath(req.body as IPathDTO) as Result<IPathDTO>;
            if(pathOrError.isFailure){
                return res.status(409).send("Path already exists");
            }
            
            
            const pathDTO = pathOrError.getValue();

            return res.status(201).json(pathDTO);
            
        }catch(e){
            next(e);
        }
    }

    public async updatePath(req: Request, res: Response, next: NextFunction) {
        try {
            const pathOrError= await this.pathService.updatePath(req.body as IPathDTO) as Result<IPathDTO>;
            if(pathOrError.isFailure){
                return res.status(404).send("Path not found");
            }
            const pathDTO= pathOrError.getValue();
            return res.json(pathDTO).status(200);
        } catch (e) {
            next(e);
        }
    }

    public async deletePath(req: Request, res: Response, next: NextFunction) {
        try {
            const pathResult = await this.pathService.deletePath(req.body.pathID);
            if(pathResult.isFailure)
                return res.status(404).send("Path not found");
            res.status(200).json(pathResult)
        } catch (e) {
            next(e);
        }
    }
}
