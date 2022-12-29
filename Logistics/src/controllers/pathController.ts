import { NextFunction, Request, Response } from "express";
import fetch from 'node-fetch';
import { Inject, Service } from "typedi";
import config from "../../config";
import { Result } from "../core/logic/Result";
import { IPathDTO } from "../dto/IPathDTO";
import IPathService from "../services/IServices/IPathService";
import IPathController from "./IControllers/IPathController";
const http = require ('https');
const jwt = require('jsonwebtoken');



@Service()
export default class PathController implements IPathController{
    constructor(
        @Inject (config.services.path.name) private pathService: IPathService,) {}

    private roles = ["admin", "logMan"];

    isAuthenticated(req: Request) {
        try {
            if(req.cookies['jwt'] == undefined)
            return false;
            const cookie = req.cookies['jwt'];
        
            const claims = jwt.verify(cookie, config.jwtSecret);
        
            if(!claims)
                return false;
            
            return true;
        } catch (error) {
            return false
        }
    
    }

    isAuthorized(req: Request, specifiedRoles?: string[]) {
        try {
            if(req.cookies['jwt'] == undefined)
                return false;
            const cookie = req.cookies['jwt'];
            const claims = jwt.verify(cookie, config.jwtSecret);
            if(!claims)
                return false;
            if(specifiedRoles != undefined){
                if(specifiedRoles.indexOf(claims.role) > -1)
                    return true;
                return false;
            }
            else if(this.roles.indexOf(claims.role) > -1)
                return true;
            return false;
        } catch (error) {
            return false;
        }

    }

    private async fetch(url : string, method: string, body: any, cookie:any, agent: any = null){
        try {
            if(body)
                return await fetch(url,{
                    method : method,
                    body : JSON.stringify(body),
                    headers: {
                        'Content-Type': 'application/json',
                        'Cookie': cookie
                    },
                    agent: agent
                });
            else
                return await fetch(url,{
                    method : method,
                    headers: {
                        'Content-Type': 'application/json',
                        'Cookie': cookie
                    },
                    agent: agent
                });
        } catch (error) {
            return {status: 503, json(): any{ return {message: "Error connecting to server"}}};
        }
    
    }
    
    public async getPath(req: Request, res: Response, next: NextFunction) {
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
        if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
        }
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
        if(req.headers.origin != "undefined"){
            if(!this.isAuthenticated(req)){
                res.status(401);
                return res.json({message: "Not authenticated"});
            }
            if(!this.isAuthorized(req)){
                res.status(403);
                return res.json({message: "Not authorized"});
            }
        }
        try {
            if(req.params.startWHId == "undefined"){
                req.params.startWHId="";
            } 
            if(req.params.destinationWHId== "undefined"){
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
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
        if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
        }
        try{
        
           
            const address_start = 'https://localhost:5001/api/warehouses/Exists/' + req.body.startWHId;
            const response_start = await this.fetch(address_start, "GET", null, req.headers.cookie)
            // const response_start = await this.fetch(address_start, req.headers.cookie)

            if(response_start.status!= 200){
                res.status(404)
                return res.send("Start Warehouse not found");
            }
                
            
            const address_destination ='https://localhost:5001/api/warehouses/Exists/' + req.body.destinationWHId;
            const response_destination = await this.fetch(address_destination, "GET", null, req.headers.cookie)
        //    const response_destination = await this.fetch(address_destination, req.headers.cookie)


           if (response_destination.status != 200){
            res.status(404)
            return res.send("Destination warehouse not found");
           }
                
            
            const pathOrError = await this.pathService.createPath(req.body as IPathDTO) as Result<IPathDTO>;
            if(pathOrError.isFailure){
                res.status(409)
                return res.send(pathOrError.error);
            }
            
            
            const pathDTO = pathOrError.getValue();
            res.status(201)
            return res.json(pathDTO);
            
        }catch(e){
            next(e);
        }
    }

    public async updatePath(req: Request, res: Response, next: NextFunction) {
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
        if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
        }
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
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
        if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
        }
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

    

    
}
