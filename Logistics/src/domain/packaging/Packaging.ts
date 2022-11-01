import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { IPackagingDTO } from "../../dto/IPackagingDTO";
import { PackagingID } from "./PackagingID";
import { TruckID } from "../truck/TruckID";
import { DeliveryID } from "./DeliveryID";
import { Position } from "./Position";


interface PackagingProps {
  packagingID: PackagingID;
  truckID: TruckID;
  deliveryID: DeliveryID;
  xPosition: Position;
  yPosition: Position;
  zPosition: Position;
}

export class Packaging extends AggregateRoot<PackagingProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get packagingID (): PackagingID {
    return this.props.packagingID;
  }

  get truckID(): TruckID {
    return this.props.truckID;
  }

  get deliveryID(): DeliveryID {
    return this.props.deliveryID;
  }

  get xPosition(): Position {
    return this.props.xPosition;
  }
  get yPosition(): Position {
    return this.props.yPosition;
  }
  get zPosition(): Position {
    return this.props.zPosition;
  }


  set truckID(truckID: TruckID) {
    this.props.truckID = truckID;
  }

  set deliveryID(deliveryID: DeliveryID) {
    this.props.deliveryID = deliveryID;
  }

  set xPosition(xPosition: Position) {
    this.props.xPosition = xPosition;
  }
  
  set yPosition(yPosition: Position) {
    this.props.yPosition = yPosition;
  }

  set zPosition(zPosition: Position) {
    this.props.zPosition = zPosition;
  }
  


  private constructor (props: PackagingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (PackagingDTO:IPackagingDTO , id?: UniqueEntityID): Result<Packaging> {

      const packaging = new Packaging({
        packagingID: PackagingID.create(PackagingDTO.packagingID).getValue(),
        truckID: TruckID.create(PackagingDTO.truckID).getValue(),
        deliveryID: DeliveryID.create(PackagingDTO.deliveryID).getValue(),
        xPosition: Position.create(PackagingDTO.xPosition).getValue(),
        yPosition: Position.create(PackagingDTO.yPosition).getValue(),
        zPosition: Position.create(PackagingDTO.zPosition).getValue(),
      }, id);
  
      return Result.ok<Packaging>(packaging);


  }



}