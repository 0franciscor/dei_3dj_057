using EletricGo.Domain.Cities;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace EletricGo.Infrastructure.Cities;

public class CityEntityTypeConfiguration : IEntityTypeConfiguration<City>
{
    public void Configure(EntityTypeBuilder<City> builder)
    {
        builder.ToTable("City", SchemaNames.EletricGo);
        builder.HasKey(c => c.Id);
        builder.OwnsOne(c => c.Name).Property(c => c.Name).HasColumnName("City_Name");

    }
}