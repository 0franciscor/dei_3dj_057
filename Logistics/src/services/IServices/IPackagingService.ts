import { Result } from "../../core/logic/Result";
import { IPackagingDTO } from "../../dto/IPackagingDTO";

export default interface IPackagingService  {
    exist(packagingID: string): Promise<Result<boolean>>;
    createPackaging(Packaging: IPackagingDTO): Promise<Result<IPackagingDTO>>;
    getPackaging(PackagingID: string): Promise<Result<IPackagingDTO>>;
    getAllPackagings(): Promise<Result<IPackagingDTO[]>>;
    updatePackaging(Packaging: IPackagingDTO): Promise<Result<IPackagingDTO>>;
    deletePackaging(PackagingID: string): Promise<Result<IPackagingDTO>>;
}
