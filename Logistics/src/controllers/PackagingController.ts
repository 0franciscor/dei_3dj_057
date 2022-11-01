import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import config from "../../config";

import IPackagingController from './IControllers/IPackagingController';
import IPackagingService from '../services/IServices/IPackagingService';
import { IPackagingDTO } from '../dto/IPackagingDTO';

import { Result } from '../core/logic/Result';



@Service()
export default class PackagingController implements IPackagingController {

    constructor(
        @Inject(config.services.packaging.name) private packagingService: IPackagingService,
    ) { }

    public async getPackaging(req: Request, res: Response, next: NextFunction){
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
        try {
            
            const packaging = await this.packagingService.getAllPackagings();
            if (packaging.isFailure)
                return res.status(404).send("Packaging not found");

            res.status(200).json(packaging);
        } catch (e) {
            next(e);
        }
    }

    public async createPackaging(req: Request, res: Response, next: NextFunction) {
        try {
            const packagingOrError = await this.packagingService.createPackaging(req.body as IPackagingDTO) as Result<IPackagingDTO>;
            if (packagingOrError.isFailure) {
                return res.status(409).send("Packaging already exists");
            }

            const packagingDTO = packagingOrError.getValue();
            
            return res.status(201).json( packagingDTO );


            } catch (e) {
            next(e);
        }
    }

    public async updatePackaging(req: Request, res: Response, next: NextFunction) {
        try {
            const packagingOrError = await this.packagingService.updatePackaging(req.body as IPackagingDTO) as Result<IPackagingDTO>;
            if (packagingOrError.isFailure) {
                return res.status(404).send("Packaging not found");
            }
            const packagingDTO = packagingOrError.getValue();
            return res.status(200).json( packagingDTO );
        } catch (e) {
            next(e);
        }
    }

    public async deletePackaging(req: Request, res: Response, next: NextFunction){
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