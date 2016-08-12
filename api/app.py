from flask import Flask, request, json
from sqlalchemy import create_engine
import pandas as pa
import json

e = create_engine('sqlite:///gages.db')

app = Flask(__name__)

# some references
#http://blog.luisrei.com/articles/flaskrest.html
#http://blog.miguelgrinberg.com/post/designing-a-restful-api-with-python-and-flask

@app.route("/")
def api_root():
    return "Hello, Earth!"

@app.route("/variables")
def api_vars():
    q = pa.read_sql("select VARIABLE_TYPE, VARIABLE_NAME, DESCRIPTION from variable_descriptions",e)
    q.VARIABLE_TYPE = q.VARIABLE_TYPE.str.lower()
    r = json.dumps({'variables': q.to_dict(orient='records')})
    return r

@app.route("/variables/types")
def api_vart():
    q = pa.read_sql("select distinct VARIABLE_TYPE from variable_descriptions",e)
    q.VARIABLE_TYPE = q.VARIABLE_TYPE.str.lower()
    r = json.dumps({'variable types': q.VARIABLE_TYPE.tolist()})
    return r


@app.route("/sites")
def api_sites():
    q = pa.read_sql("select STAID, STANAME from conterm_basinid",e)
    r = json.dumps({'sites': q.to_dict(orient='records')})
    return r

#stanum = '14378200'
@app.route("/data")
def api_data():
    stanum = request.args['s'] # sites
    if 'v' in request.args:
        var = ['conterm_'+v for v in request.args['v'].split(',') if v != 'basinid']
        frm = 'conterm_basinid,'+','.join(var)
        whr = 'conterm_basinid.STAID = ' + stanum + ' AND conterm_basinid.STAID = ' + '.STAID AND conterm_basinid.STAID = '.join(var) + '.STAID'
    else: # get all of the data
        frm = 'conterm_basinid,\
            conterm_bas_classif,\
            conterm_bas_morph,\
            conterm_bound_qa,\
            conterm_climate,\
            conterm_climate_ppt_annual,\
            conterm_climate_tmp_annual,\
            conterm_flowrec,\
            conterm_geology,\
            conterm_hydro,\
            conterm_hydromod_dams,\
            conterm_hydromod_other,\
            conterm_landscape_pat,\
            conterm_lc06_basin,\
            conterm_lc06_mains100,\
            conterm_lc06_mains800,\
            conterm_lc06_rip100,\
            conterm_lc06_rip800,\
            conterm_lc_crops,\
            conterm_nutrient_app,\
            conterm_pest_app,\
            conterm_prot_areas,\
            conterm_regions,\
            conterm_soils,\
            conterm_topo'
        whr = 'conterm_basinid.STAID = ' + stanum + ' AND\
            conterm_basinid.STAID = conterm_bas_classif.STAID AND\
            conterm_basinid.STAID = conterm_bas_morph.STAID AND\
            conterm_basinid.STAID = conterm_bound_qa.STAID AND\
            conterm_basinid.STAID = conterm_climate.STAID AND\
            conterm_basinid.STAID = conterm_climate_ppt_annual.STAID AND\
            conterm_basinid.STAID = conterm_climate_tmp_annual.STAID AND\
            conterm_basinid.STAID = conterm_flowrec.STAID AND\
            conterm_basinid.STAID = conterm_geology.STAID AND\
            conterm_basinid.STAID = conterm_hydro.STAID AND\
            conterm_basinid.STAID = conterm_hydromod_dams.STAID AND\
            conterm_basinid.STAID = conterm_hydromod_other.STAID AND\
            conterm_basinid.STAID = conterm_landscape_pat.STAID AND\
            conterm_basinid.STAID = conterm_lc06_basin.STAID AND\
            conterm_basinid.STAID = conterm_lc06_mains100.STAID AND\
            conterm_basinid.STAID = conterm_lc06_mains800.STAID AND\
            conterm_basinid.STAID = conterm_lc06_rip100.STAID AND\
            conterm_basinid.STAID = conterm_lc06_rip800.STAID AND\
            conterm_basinid.STAID = conterm_lc_crops.STAID AND\
            conterm_basinid.STAID = conterm_nutrient_app.STAID AND\
            conterm_basinid.STAID = conterm_pest_app.STAID AND\
            conterm_basinid.STAID = conterm_prot_areas.STAID AND\
            conterm_basinid.STAID = conterm_regions.STAID AND\
            conterm_basinid.STAID = conterm_soils.STAID AND\
            conterm_basinid.STAID = conterm_topo.STAID'
    q = pa.read_sql('select * from '+frm+' where '+whr,e)
    r = json.dumps({'data': q.to_dict(orient='records')})
    return r

if __name__ == '__main__':
     app.run()
