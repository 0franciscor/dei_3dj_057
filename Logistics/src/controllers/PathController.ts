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
            if (path.isFailure){
                res.status(404)
                return res.send("Path not found")
            }
            res.status(200)
            return res.json(path.getValue());
        } catch (e) {
            next(e);
        }
    }

    public async getAllPaths(req: Request, res: Response, next: NextFunction) {
        try {
            if(req.params.startWHId == "undefined"){
                req.params.startWHId="";
            }else if(req.params.destinationWHId== "undefined"){
                req.params.destinationWHId="";
            }

            let warehouses= {
                startWHId: req.params.startWHId,
                destinationWHId:req.params.destinationWHId
            }as IPathDTO;

            const paths= await this.pathService.getAllPath(warehouses);
            if(paths.getValue().length==0){
                res.status(404) 
                return res.send("No paths found");
            }
            res.status(200)
            return res.json(paths.getValue());
        } catch (e) {
            next(e);
        }
    }

    public async createPath(req: Request, res: Response, next: NextFunction) {
        try{
        
           
            const address_start = 'https://localhost:5001/api/warehouses/Exists/' + req.body.startWHId;

            const response_start = await this.fetch(address_start)

            if(response_start.status==404){
                res.status(404)
                return res.send("Start Warehouse not found");
            }
                
            
            const address_destination ='https://localhost:5001/api/warehouses/Exists/' + req.body.destinationWHId;
           const response_destination = await this.fetch(address_destination)


           if (response_destination.status == 404){
            res.status(404)
            return res.send("Destination warehouse not found");
           }
                
            
            const pathOrError = await this.pathService.createPath(req.body as IPathDTO) as Result<IPathDTO>;
            if(pathOrError.isFailure){
                res.status(409)
                return res.send("Path already exists");
            }
            
            
            const pathDTO = pathOrError.getValue();
            res.status(201)
            return res.json(pathDTO);
            
        }catch(e){
            next(e);
        }
    }

    public async updatePath(req: Request, res: Response, next: NextFunction) {
        try {
            if(req.body.pathID == null){
                res.status(400);
                return res.send("Path ID is required")
            }
                
            const pathOrError= await this.pathService.updatePath(req.body as IPathDTO) as Result<IPathDTO>;
            if(pathOrError.isFailure){
                
                if(pathOrError.error == "Path not found"){
                    res.status(404)
                    return res.send(pathOrError.error);
                }
                res.status(400)
                return res.send(pathOrError.error);
            }
            const pathDTO= pathOrError.getValue();
            res.status(200)
            return res.json(pathDTO);
        } catch (e) {
            next(e);
        }
    }

    public async deletePath(req: Request, res: Response, next: NextFunction) {
        try {
            const pathResult = await this.pathService.deletePath(req.body.id);
            if(pathResult.isFailure){
                res.status(404);
                return res.send("Path not found");
            }
            res.status(200)
            return res.json(pathResult.getValue())
        } catch (e) {
            next(e);
        }
    }

    private async fetch(address : string ){
        const httpAgent = new http.Agent({rejectUnauthorized: false});
        return await fetch(address,{
        method : 'GET',
        agent: httpAgent
        });
    }
}
