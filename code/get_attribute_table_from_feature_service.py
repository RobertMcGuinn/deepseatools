import arcpy
import os

# Set environment
arcpy.env.overwriteOutput = True

# FIXED: Use the layer index (e.g., /0)
feature_layer_url = "https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/DSCRTP_NatDB/FeatureServer/0"

# Output CSV path
output_csv = r"C:\rworking\deepseatools\indata\feature_service.csv"

# Create a temporary GDB
scratch_folder = arcpy.env.scratchFolder
temp_gdb = os.path.join(scratch_folder, "temp_export.gdb")
if not arcpy.Exists(temp_gdb):
    arcpy.CreateFileGDB_management(scratch_folder, "temp_export.gdb")

temp_fc = os.path.join(temp_gdb, "coral_temp")

# Export features to temporary GDB
arcpy.FeatureClassToFeatureClass_conversion(
    in_features=feature_layer_url,
    out_path=temp_gdb,
    out_name="coral_temp"
)

# Export the attribute table to CSV
arcpy.TableToTable_conversion(
    in_rows=temp_fc,
    out_path=os.path.dirname(output_csv),
    out_name=os.path.basename(output_csv)
)

print(f"Export complete: {output_csv}")
