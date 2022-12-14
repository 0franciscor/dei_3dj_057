import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import querystring from 'querystring';
import config from "../../config";

import IPackagingController from './IControllers/IPackagingController';
import IPackagingService from '../services/IServices/IPackagingService';
import ITruckService from '../services/IServices/ITruckService';
import { IPackagingDTO } from '../dto/IPackagingDTO';

import { Result } from '../core/logic/Result';
import fetch from 'node-fetch';

const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class PackagingController implements IPackagingController {

    constructor(
        @Inject(config.services.packaging.name) private packagingService: IPackagingService,
        @Inject(config.services.truck.name) private truckService: ITruckService
    ) { }

    private roles = ["admin","logMan"];

    isAuthenticated(req: Request) {
        if(req.cookies['jwt'] == undefined)
            return false;
        const cookie = req.cookies['jwt'];
        const claims = jwt.verify(cookie, config.jwtSecret);
        if(!claims)
            return false;
        
        return true;
    }

    isAuthorized(req: Request) {
        if(req.cookies['jwt'] == undefined)
        return false;
        const cookie = req.cookies['jwt'];
        const claims = jwt.verify(cookie, config.jwtSecret);
        if(!claims)
            return false;
        if(this.roles.indexOf(claims.role) > -1)
            return true;
        return false;
    }

    public async getPackaging(req: Request, res: Response, next: NextFunction){
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
        if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
        }
        try {
            
            const packaging = await this.packagingService.getPackaging(req.body.packagingID);
            if (packaging.isFailure)
                return res.status(404).send("Packaging not found");
            
            res.status(200).json(packaging);
        } catch (e) {
            next(e);
        }
    }

    public async getAllPackagings(req: Request, res: Response, next: NextFunction){
        
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
          console.log("getAllPackagings")
        if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
          }
        try {
            
            const packaging = await this.packagingService.getAllPackagings();

            if (packaging.isFailure)
                return res.status(404).send("Packaging not found");

            res.status(200).json(packaging.getValue());
            
        } catch (e) {
            next(e);
        }
    }


    
    public async createPackaging(req: Request, res: Response, next: NextFunction) {
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
          if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
          }
        try {
           
            if(req.body.packagingID == null)
                return res.status(400).send("PackagingID is required");
            const httpAgent = new http.Agent({ rejectUnauthorized: false });
            const address = 'https://localhost:5001/api/deliveries/Exists/' + req.body.deliveryID;

            
            const response = await fetch(address, {
                method: 'GET',
                agent: httpAgent
            });


            if (response.status == 404)
                return res.status(404).send("Delivery not found");


            const truckOrError = await this.truckService.exist(req.body.truckID);
            if (truckOrError.getValue() == false) {
                return res.status(404).send("Truck not found");
            }


            const packagingOrError = await this.packagingService.createPackaging(req.body as IPackagingDTO) as Result<IPackagingDTO>;
            
            if (packagingOrError.isFailure) {
                return res.status(409).send("Packaging already exists");
            }

            const packagingDTO = packagingOrError.getValue();

            console.log(packagingDTO);
            
            return res.status(201).json( packagingDTO );


            } catch (e) {
            next(e);
        }
    }

    public async updatePackaging(req: Request, res: Response, next: NextFunction) {
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
          if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
          }
        try {
            if(req.body.packagingID == null)
                return res.status(400).send("PackagingID is required");
            const packagingOrError = await this.packagingService.updatePackaging(req.body as IPackagingDTO);
            if (packagingOrError.isFailure) {
                if (packagingOrError.error == "TruckID cannot be changed")
                    return res.status(400).send(packagingOrError.error);
                if (packagingOrError.error == "DeliveryID cannot be changed")
                    return res.status(400).send(packagingOrError.error);
                if (packagingOrError.error == "Packaging not found")
                    return res.status(404).send(packagingOrError.error);
                return res.status(400).send(packagingOrError.error);
            }
            const packagingDTO = packagingOrError.getValue();
            
            return res.status(200).json( packagingDTO );
        } catch (e) {
            next(e);
        }
    }

    public async deletePackaging(req: Request, res: Response, next: NextFunction){
        if(!this.isAuthenticated(req)){
            res.status(401);
            return res.json({message: "Not authenticated"});
          }
          if(!this.isAuthorized(req)){
            res.status(403);
            return res.json({message: "Not authorized"});
          }
        try {

            const packagingResult = await this.packagingService.deletePackaging(req.body.packagingID);
            if(packagingResult.isFailure)
                return res.status(404).send("Packaging not found");
            res.status(200).json(packagingResult);
        } catch (e) {
            next(e);
        }
    }

}