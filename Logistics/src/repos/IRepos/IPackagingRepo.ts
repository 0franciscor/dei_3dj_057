import { Repo } from '../../core/infra/Repo';
import { Packaging } from '../../domain/packaging/Packaging';
import { PackagingID } from '../../domain/packaging/PackagingID';

export default interface IPackagingRepo extends Repo<Packaging> {
    exists(Packaging: Packaging): Promise<boolean>;
    save(Packaging: Packaging): Promise<Packaging>;
    delete(Packaging: Packaging): Promise<Packaging>;
    getPackagingById(PackagingID: PackagingID|string): Promise<Packaging>;
    getAllPackagings(): Promise<Packaging[]>;
    
}   