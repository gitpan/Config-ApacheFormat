use ExtUtils::MakeMaker;
use 5.006001;
WriteMakefile(
    NAME		=> 'Config::ApacheFormat',
    VERSION_FROM	=> 'ApacheFormat.pm',
    PREREQ_PM		=> {
                            'Scalar::Util'       => "1.07",
                            'Class::MethodMaker' => "1.08",
                            'Carp'               => 0,
                            'Text::Balanced'     => "1.89",
                            'File::Spec'         => "0.82",
                           }, 
    ABSTRACT_FROM       => 'ApacheFormat.pm',
    AUTHOR              => 'Sam Tregar <sam@tregar.com>',
);
