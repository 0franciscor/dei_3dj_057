import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Autonomy } from "./Autonomy";
import { Capacity } from "./Capacity";
import { FastChargeTime } from "./FastChargeTime";
import { MaxBatteryCapacity } from "./MaxBatteryCapacity";
import { Tare } from "./Tare";
import { TruckID } from "./TruckID";


interface TruckProps {
  tare: Tare;
  capacity: Capacity;
  maxBatteryCapacity: MaxBatteryCapacity;
  autonomy: Autonomy;
  fastChargeTime: FastChargeTime;

}

export class Truck extends AggregateRoot<TruckProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get truckID (): TruckID {
    return TruckID.caller(this.id)
  }

  get capacity (): Capacity {
    return this.props.capacity;
  }

  get maxBatteryCapacity (): MaxBatteryCapacity {
    return this.props.maxBatteryCapacity;
  }

  get autonomy (): Autonomy {
    return this.props.autonomy;
  }

  get fastChargeTime (): FastChargeTime {
    return this.props.fastChargeTime;
  }

  private constructor (props: TruckProps, id?: UniqueEntityID) {
    super(props, id);
  }

}