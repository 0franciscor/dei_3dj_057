import { Mapper } from "../core/infra/Mapper";

import { Document, Model } from 'mongoose';

import { IPackagingDTO } from "../dto/IPackagingDTO";
import { Packaging } from "../domain/packaging/Packaging";

import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { IPackagingPersistence } from "../dataschema/IPackagingPersistence";

export class PackagingMap extends Mapper<Packaging> {
    
    public static toDTO(Packaging: Packaging): IPackagingDTO {
        return {
            id: Packaging.id.toString(),
            packagingID: Packaging.packagingID.id,
            truckID: Packaging.truckID.id,
            deliveryID: Packaging.deliveryID.id,
            xPosition: Packaging.xPosition.XPosition,
            yPosition: Packaging.yPosition.YPosition,
            zPosition: Packaging.zPosition.ZPosition
        } as IPackagingDTO;
    }

    public static toDTOList(Packagings: Packaging[]): IPackagingDTO[] {
        return Packagings.map((Packaging) => PackagingMap.toDTO(Packaging));
    }

    public static toDomain(packaging: any | Model<IPackagingPersistence & Document>): Packaging {
        const PackagingOrError = Packaging.create(
                packaging,
                new UniqueEntityID(packaging.id),
        );
        PackagingOrError.isFailure ? console.log(PackagingOrError.error) : '';
        
        return PackagingOrError.isSuccess ? PackagingOrError.getValue() : null;
    }

    public static toPersistence(Packaging: Packaging): any {
        return {
            domainId: Packaging.id.toString(),
            packagingID: Packaging.packagingID.id,
            deliveryID: Packaging.deliveryID.id,
            truckID: Packaging.truckID.id,
            xPosition: Packaging.xPosition.XPosition,
            yPosition: Packaging.yPosition.YPosition,
            zPosition: Packaging.zPosition.ZPosition
        };
    }
}