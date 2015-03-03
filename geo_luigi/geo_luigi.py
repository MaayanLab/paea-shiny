# -*- coding: utf-8 -*-

from __future__ import print_function
import csv
import os
import luigi
import logging
import rpy2.robjects.packages as rpackages
workflows = rpackages.importr('geoWorkflows')

class GetGEO(luigi.Task):
    '''Download GEO file and extract annotated expression data
    :param geo_id
    :param destdir directory to store soft files
    :param outputdir directory to store output
    :param control list of control ids
    :param treatment list of treatment/perturbation ids
    :param description string with dataset description
    '''
    geo_id = luigi.Parameter()
    destdir = luigi.Parameter()
    outputdir = luigi.Parameter() 
    control = luigi.Parameter()
    treatment = luigi.Parameter()
    description = luigi.Parameter()


    def process_gse(self):
        return workflows.process_gse(
            geo_id=self.geo_id,
            destdir=self.destdir,
            outputdir=self.outputdir,
            control=list(self.control),
            treatment=list(self.treatment),
            description=self.description
        )

    
    def output(self):
        return luigi.LocalTarget(os.path.join(self.outputdir, '{0}.list'.format(self.geo_id)))

    
    def run(self):
        if len(self.control) > 1 and len(self.treatment) > 1:
            try:
                expr_files = [_ for _ in self.process_gse()]
                with self.output().open('w') as fw:
                    for expr_file in expr_files:
                        print(expr_file, file=fw)


            except Exception as e:
                logging.exception('{0}'.format(self.geo_id))
                with self.output().open('w') as fw:
                    pass

        else:
            with self.output().open('w') as fw:
                pass



class AllDiseases(luigi.Task):
    '''
    :param input_path path to the input file
    :param destdir  directory to store soft files
    :param outputdir directory to store output
    '''
    input_path = luigi.Parameter()
    destdir = luigi.Parameter()
    outputdir = luigi.Parameter() 
 
    fieldnames = ('geo_id', 'disease', 'ctrl_ids', 'pert_ids', 'platform', 'cell_type')


    def process_row(self, row):
        ctrl_ids = row['ctrl_ids'].split(',')
        pert_ids = row['pert_ids'].split(',')
        geo_id = row['geo_id']
        desc = '{0}\t{1}'.format(row['disease'], row['cell_type'])

        return GetGEO(geo_id, self.destdir, self.outputdir, ctrl_ids, pert_ids, desc)


    def output(self):
        return luigi.LocalTarget(os.path.join(self.outputdir, 'meta.csv'))


    def data_description(self):
        with open(self.input_path) as fr:
            reader = csv.DictReader(fr, delimiter='\t', fieldnames=self.fieldnames)
            return [_ for _ in reader]


    def requires(self):
        return [self.process_row(row) for row in self.data_description()]


    def run(self):

        with self.output().open('w') as fw:
            writer = csv.writer(fw)
            for inp in self.input():
                with inp.open('r') as fr:
                    lines = fr.readlines()
                if len(lines) == 1:
                    key = os.path.split(inp.path)[-1].split('.')[0]
                    value = os.path.split(lines[0])[-1].strip()
                    writer.writerow((key, value))


if __name__ == '__main__':
    luigi.run()
