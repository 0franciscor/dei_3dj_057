import { Service, Inject } from 'typedi';

import IPackagingRepo from './IRepos/IPackagingRepo';
import { Packaging } from '../domain/packaging/Packaging';
import { IPackagingPersistence } from '../dataschema/IPackagingPersistence';
import { PackagingMap } from '../mappers/PackagingMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { PackagingID } from '../domain/packaging/PackagingID';


@Service()
export default class PackagingRepo implements IPackagingRepo {
    
    constructor(
        @Inject('packagingSchema') private PackagingSchema : Model<IPackagingPersistence & Document>,
    ) { }

    public async exists(Packaging: Packaging): Promise<boolean> {
        const idX = Packaging.id instanceof PackagingID ? (<PackagingID>Packaging.id) : Packaging.id;

        const query = { domainId: idX}; 
        const PackagingDocument = await this.PackagingSchema.findById( query as FilterQuery<IPackagingPersistence & Document>);

        return !!PackagingDocument === true;
    }

    public async save(Packaging: Packaging): Promise<Packaging> {
        const query = { packagingID: Packaging.packagingID.id};
        const PackagingDocument = await this.PackagingSchema.findOne( query as FilterQuery<IPackagingPersistence & Document> );
        try {
            if(PackagingDocument === null) {
                const rawPackaging: any = PackagingMap.toPersistence(Packaging);
                const PackagingCreated = await this.PackagingSchema.create(rawPackaging);
                return PackagingMap.toDomain(PackagingCreated);
            }
            else{
                PackagingDocument.packagingID = Packaging.packagingID.id;
                await PackagingDocument.save();
                return Packaging;
            
            }
        } catch (error) {
            throw error;
        }

    }

    public async delete(Packaging: Packaging): Promise<Packaging> {
        const query = { packagingID: Packaging.packagingID.id};
        const PackagingDocument = await this.PackagingSchema.findOne( query as FilterQuery<IPackagingPersistence & Document> );
        try {
            if(PackagingDocument === null) {
                return Packaging;
            }
            else{
                await this.PackagingSchema.deleteOne( query as FilterQuery<IPackagingPersistence & Document> );
                return Packaging;
            }

        } catch (error) {
            throw error;
        }
    }

    public async getPackagingById(id: string): Promise<Packaging> {
        const query = { packagingID: id};
        const PackagingDocument = await this.PackagingSchema.findOne( query as FilterQuery<IPackagingPersistence & Document> );
        if(PackagingDocument === null) {
            return null;
        }
        else{
            
            return PackagingMap.toDomain(PackagingDocument);
        }
    }
    public async getAllPackagings(): Promise<Packaging[]> {
        const PackagingsDocument = await this.PackagingSchema.find();

        let Packagings: Packaging[] = [];
        PackagingsDocument.forEach(Packaging => {
            Packagings.push(PackagingMap.toDomain(Packaging));
        });
        return Packagings;
    }



}