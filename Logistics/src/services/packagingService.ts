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
import { Position } from '../domain/packaging/Position';
import { Truck } from '../domain/truck/Truck';

@Service()
export default class PackagingService implements IPackagingService {
    constructor(
        @Inject(config.repos.packaging.name) private packagingRepo: IPackagingRepo,
    ) { }

    
    public async createPackaging(packagingDTO: IPackagingDTO): Promise<Result<IPackagingDTO>> {
        try {
            const packaging = await this.packagingRepo.getPackagingById(packagingDTO.packagingID);
            if(packaging !== null)
                return Result.fail<IPackagingDTO>("Packaging already exists");
            
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
            console.log(packaging)
            const packagingDTOResult = PackagingMap.toDTO(packaging) as IPackagingDTO;
            console.log(packagingDTOResult)
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
            /*
            if(packagingDTO.truckID !== packaging.truckID.id && packagingDTO.packagingID !== null)
                packaging.truckID = TruckID.create(packagingDTO.truckID).getValue();
            if(packagingDTO.deliveryID !== packaging.deliveryID.id && packagingDTO.deliveryID !== null)
                packaging.deliveryID = DeliveryID.create(packagingDTO.deliveryID).getValue();
            */
           
            if(packagingDTO.xPosition !== packaging.xPosition.Position && packagingDTO.xPosition !== null)
                packaging.xPosition = Position.create(packagingDTO.xPosition).getValue();
            if(packagingDTO.yPosition !== packaging.yPosition.Position && packagingDTO.yPosition !== null)
                packaging.yPosition = Position.create(packagingDTO.yPosition).getValue();
            if(packagingDTO.zPosition !== packaging.zPosition.Position && packagingDTO.zPosition !== null)
                packaging.zPosition = Position.create(packagingDTO.zPosition).getValue();

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