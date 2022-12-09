using EletricGo.Domain.Warehouses;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace EletricGo.Infrastructure.Warehouses
{
    internal class WarehouseEntityTypeConfiguration : IEntityTypeConfiguration<Warehouse>
    {

        public void Configure(EntityTypeBuilder<Warehouse> builder)
        {
            builder.ToTable("Warehouse", SchemaNames.EletricGo);
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.Address).Property(b => b.fullAddress).HasColumnName("Address");
            builder.OwnsOne(b => b.Designation).Property(b => b.designation).HasColumnName("Designation");
            builder.OwnsOne(b => b.Altitude).Property(b => b.altitude).HasColumnName("Altitude");
            builder.OwnsOne(b => b.Coordinates).Property(b => b.latitude).HasColumnName("Latitude");
            builder.OwnsOne(b => b.Coordinates).Property(b => b.longitude).HasColumnName("Longitude");
            builder.OwnsOne((b => b.cityId)).Property(b=>b.Id).HasColumnName("City_Id");
            builder.Property(b => b.active).HasColumnName("Active");

        }
    }    
}
