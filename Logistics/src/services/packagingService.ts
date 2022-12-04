import { Service, Inject } from 'typedi';
import { Result } from "../core/logic/Result";
import config from "../../config";
import { IPackagingDTO } from "../dto/IPackagingDTO";
import IPackagingRepo from '../repos/IRepos/IPackagingRepo';
import IPackagingService from './IServices/IPackagingService';
import { Packaging } from "../domain/packaging/Packaging";
import { PackagingMap } from '../mappers/PackagingMap';

import { PackagingID } from '../domain/packaging/PackagingID';
import { TruckID } from '../domain/truck/TruckID';
import { DeliveryID } from '../domain/packaging/DeliveryID';
import { XPosition } from '../domain/packaging/XPosition';
import { YPosition } from '../domain/packaging/YPosition';
import { ZPosition } from '../domain/packaging/ZPosition';
import { Truck } from '../domain/truck/Truck';

@Service()
export default class PackagingService implements IPackagingService {
    constructor(
        @Inject(config.repos.packaging.name) private packagingRepo: IPackagingRepo,
    ) { }


    public async exist(packagingID: string): Promise<Result<boolean>> {
        try {
            const packagingResult = await this.packagingRepo.getPackagingById(packagingID);
            if(packagingResult === null)
                return Result.ok<boolean>(false);
            return Result.ok<boolean>(true);
        } catch (e) {
            throw e;
        }
    }

    
    public async createPackaging(packagingDTO: IPackagingDTO): Promise<Result<IPackagingDTO>> {
        try {
            
            const packaging = await this.packagingRepo.getPackagingById(packagingDTO.packagingID);
            
            if(packaging !== null){
                
                return Result.fail<IPackagingDTO>("Packaging already exists");
            }
                
            const packagingOrError = Packaging.create(packagingDTO);
            
            if (packagingOrError.isFailure) {
                return Result.fail<IPackagingDTO>(packagingOrError.error);
            }
            const packagingResult = packagingOrError.getValue();
            await this.packagingRepo.save(packagingResult);
            
            const packagingDTOResult = PackagingMap.toDTO(packagingResult) as IPackagingDTO;
            return Result.ok<IPackagingDTO>(packagingDTOResult);


        } catch (e) {
            
            throw e;

        }
    }

    public async getPackaging(packagingID: string): Promise<Result<IPackagingDTO>> {
        try { 

            const packaging = await this.packagingRepo.getPackagingById(packagingID);
            
            if(packaging === null)
                return Result.fail<IPackagingDTO>("Packaging not found");
            
            const packagingDTOResult = PackagingMap.toDTO(packaging) as IPackagingDTO;
            return Result.ok<IPackagingDTO>(packagingDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async getAllPackagings(): Promise<Result<IPackagingDTO[]>> {

        try {
            const packagings = await this.packagingRepo.getAllPackagings();
            const packagingDTOResult = PackagingMap.toDTOList(packagings) as IPackagingDTO[];
            return Result.ok<IPackagingDTO[]>(packagingDTOResult);
        } catch (e) {
            throw e;
        }

    }

    public async updatePackaging(packagingDTO: IPackagingDTO): Promise<Result<IPackagingDTO>> {
        try {
            
            const packaging = await this.packagingRepo.getPackagingById(packagingDTO.packagingID);
            if(packaging === null)
                return Result.fail<IPackagingDTO>("Packaging not found");
            
            if(packagingDTO.truckID !== packaging.truckID.id && packagingDTO.truckID !== undefined)
                return Result.fail<IPackagingDTO>("TruckID cannot be changed");
            if(packagingDTO.deliveryID !== packaging.deliveryID.id && packagingDTO.deliveryID !== undefined)
                return Result.fail<IPackagingDTO>("DeliveryID cannot be changed");
            
            

            if(packagingDTO.xPosition !== packaging.xPosition.XPosition && packagingDTO.xPosition !== undefined){
                const xPositionOrError = XPosition.create(packagingDTO.xPosition);
                if (xPositionOrError.isFailure) {
                    return Result.fail<IPackagingDTO>(xPositionOrError.error);
                }
                packaging.xPosition = xPositionOrError.getValue();
            }
            if(packagingDTO.yPosition !== packaging.yPosition.YPosition && packagingDTO.yPosition !== undefined){
                const yPositionOrError = YPosition.create(packagingDTO.yPosition);
                if (yPositionOrError.isFailure) {
                    return Result.fail<IPackagingDTO>(yPositionOrError.error);
                }
                packaging.yPosition = yPositionOrError.getValue();
            }
            if(packagingDTO.zPosition !== packaging.zPosition.ZPosition && packagingDTO.zPosition !== undefined){
                const zPositionOrError = ZPosition.create(packagingDTO.zPosition);
                if (zPositionOrError.isFailure) {
                    return Result.fail<IPackagingDTO>(zPositionOrError.error);
                }
                packaging.zPosition = zPositionOrError.getValue();

            }

            
            await this.packagingRepo.save(packaging);

            const PackagingDTOResult = PackagingMap.toDTO(packaging) as IPackagingDTO;
            return Result.ok<IPackagingDTO>(PackagingDTOResult);
        } catch (e) {
            throw e;
        }

    }

    public async deletePackaging(packagingID: string): Promise<Result<IPackagingDTO>> {
        try {

            const Packaging = await this.packagingRepo.getPackagingById(packagingID);
            if(Packaging === null)
                return Result.fail<IPackagingDTO>("Packaging not found");

            await this.packagingRepo.delete(Packaging);

            const PackagingDTOResult = PackagingMap.toDTO(Packaging) as IPackagingDTO;
            return Result.ok<IPackagingDTO>(PackagingDTOResult);
        } catch (e) {
            throw e;
        }
    }


}