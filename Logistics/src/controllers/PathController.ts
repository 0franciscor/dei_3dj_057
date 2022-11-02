import config from "../../config";
import { Inject, Service } from "typedi";
import  IPathController  from "../controllers/IControllers/IPathController"
import IPathService from "../services/IServices/IPathService";
import { Request, Response, NextFunction } from "express";
import { IPathDTO } from "../dto/IPathDTO";
import { Result } from "../core/logic/Result";
import { http } from "winston";





@Service()
export default class PathController implements IPathController{
    constructor(
        @Inject (config.services.path.name) private pathService: IPathService,) {}
    
    public async getPath(req: Request, res: Response, next: NextFunction) {
        try {
            const path = await this.pathService.getPath(req.body.pathID);
            res.status(200).json(path);
        } catch (e) {
            next(e);
        }
    }

    public async getAllPaths(req: Request, res: Response, next: NextFunction) {
        try {
            const paths= await this.pathService.getAllPath();
        } catch (e) {
            next(e);
        }
    }

    public async createPath(req: Request, res: Response, next: NextFunction) {
        const https = require("https");
        try{
            
            https.get("https://localhost:5001/api/warehouses/"+req.body.startWHId, res =>{
                console.log(res.statusCode);
            })

            const pathOrError = await this.pathService.createPath(req.body as IPathDTO) as Result<IPathDTO>;
        
            if(pathOrError.isFailure){
                return res.status(403).send("Path already exists");
            }

            const pathDTO = pathOrError.getValue();
            return res.json(pathDTO).status(201);
        }catch(e){
            next(e);
        }
    }

    public async updatePath(req: Request, res: Response, next: NextFunction) {
        try {
            const pathOrError= await this.pathService.updatePath(req.body as IPathDTO) as Result<IPathDTO>;
            if(pathOrError.isFailure){
                return res.status(403).send("Path not found");
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
                return res.status(403).send("Path not found");
            res.status(200).json(pathResult)
        } catch (e) {
            next(e);
        }
    }
}
