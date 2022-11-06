import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface WarehouseProps {
    warehouse: string;
}

export class Warehouses extends ValueObject<WarehouseProps>
{
    get warehouse (): string {
        return this.props.warehouse;
    }

    private constructor (props: WarehouseProps){
        super(props);
    }

    public static create (warehouse: string): Result<Warehouses> {
        if(warehouse.length <= 0) {
            return Result.fail<Warehouses>('Warehouse must be greater than 0');

        }

        return Result.ok<Warehouses>(new Warehouses({ warehouse}));
    }




}