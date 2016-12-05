extern crate jimn;
extern crate argparse;
use argparse::{ArgumentParser, Store};
//use jimn::compute_milling_path;


fn main() {
    let mut thickness = 0.4;
    let mut milling_radius = 0.2;
    let mut stl_file = String::new();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Convert stl file to gcode for 2.5D milling.");
        ap.refer(&mut thickness)
            .add_option(&["-t", "--thickness"], Store,
                        "thickness of each horizontal slice");
        ap.refer(&mut milling_radius)
            .add_option(&["-r", "--milling_radius"], Store,
                        "radius used for milling model.");
        ap.refer(&mut stl_file)
            .add_argument("stl_file", Store, "filename of STL model");
        ap.parse_args_or_exit();
    }
    if stl_file != "" {
        println!("TODO");
        //compute_milling_path(thickness, milling_radius, stl_file);
    } else {
        println!("missing stl file, see help")
    }
}
