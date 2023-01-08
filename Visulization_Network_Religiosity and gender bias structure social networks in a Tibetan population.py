import pandas as pd
import networkx as nx
from pyecharts import options as opts
from pyecharts.charts import Graph
from snapshot_phantomjs import snapshot
from pyecharts.render import make_snapshot


def plot_data_process(node_list, edge_list, gender_df, data):
    g = nx.Graph()
    g.add_nodes_from(node_list)
    g.add_edges_from(edge_list)
    degree_df = pd.DataFrame(g.degree())
    degree_df.columns = ['id', 'degree']
    network_df = degree_df.merge(gender_df, left_on='id', right_on='ID', how='left')
    nodes_df = network_df[['id', 'degree', 'Gender']]
    nodes_df.columns = ['id', 'symbolSize', 'category']
    nodes_df['itemStyle'] = nodes_df['category'].apply(lambda x: {"normal": {"color": '#DB7093'}} \
        if x == 'Female' else {"normal": {"color": '#6CA6CD'}})
    nodes_df = nodes_df[nodes_df['category'].notnull()]
    nodes_df = nodes_df.sort_values(by=['symbolSize', 'category']).reset_index(drop=True)
    nodes_df['category'] = nodes_df['category'].apply(lambda x: 0 if x == 'Female' else 1)
    links_df = data[['Ego_str', 'Alter_str', 'Support']].copy()
    links_df.columns = ['source', 'target', 'value']
    node_list_new = []
    for index, each_node in nodes_df.iterrows():
        node_list_new.append(dict(each_node))
    link_list_new = []
    for index, each_link in links_df.iterrows():
        link_list_new.append(dict(each_link))
    return node_list_new, link_list_new


def plot_func(node_list, link_list, categories, file_name):
    c = (
        Graph(init_opts=opts.InitOpts(bg_color='#FFFFFF', width="1000px", height="1000px"))
            .add(
            "",
            nodes=node_list,
            links=link_list,
            categories=categories,
            gravity=0.5,
            repulsion=100,
            layout="circular",
            is_rotate_label=True,
            symbol='circle',
            edge_symbol='arrow',
            edge_symbol_size=3,
            linestyle_opts=opts.LineStyleOpts(color='target', curve=0.3),
            itemstyle_opts=opts.ItemStyleOpts(border_color='black'),
            label_opts=opts.LabelOpts(position="right"),
        )
            .set_global_opts(
            title_opts=opts.TitleOpts(title=file_name, pos_left="45%", pos_top="5%",
                                      title_textstyle_opts=opts.TextStyleOpts(font_size=20)),
            legend_opts=opts.LegendOpts(orient="vertical", pos_right="5%", pos_top="10%", item_width=50, item_height=50,
                                        legend_icon='circle'),
        )
    )
    make_snapshot(snapshot, c.render(), "{}.png".format(file_name))


if __name__ == '__main__':
    # 读取网络数据
    data = pd.read_csv("./Analyzed_Data_ML/Social_network.csv")
    data['Ego_str'] = data['Ego'].apply(lambda x: str(x))
    data['Alter_str'] = data['Alter'].apply(lambda x: str(x))
    # 读取节点信息数据
    node_data = pd.read_csv('./Analyzed_Data_ML/Nodes_attributes.csv', index_col=0)
    node_data['ID'] = node_data['ID'].apply(lambda x: str(x))
    gender_df = node_data[['ID', 'Gender']].copy().reset_index(drop=True)
    link_class_list = list(data['Support'].unique())
    name_dict = {'Emotionally': 'Emotional', 'Physically': 'Behavioural', 'Financially': 'Financial',
                 'Suggestion': 'Guidance', 'Guaranty': 'Guarantee'}
    categories = [
        {
            "name": "Male",
            'itemStyle': {'normal': {'color': '#6CA6CD'}}
        },
        {
            "name": "Female",
            'itemStyle': {'normal': {'color': '#DB7093'}}
        }]
    for each_link in link_class_list:
        file_name = name_dict.get(each_link)
        part_data = data[data['Support'] == each_link].copy()
        ego = list(part_data['Ego_str'])
        alter = list(part_data['Alter_str'])
        node_list = list(set(ego).union(set(alter)))
        edge_list = []
        for each in zip(ego, alter):
            edge_list.append(each)
        node_list_new, link_list_new = plot_data_process(node_list, edge_list, gender_df, part_data)
        plot_func(node_list_new, link_list_new, categories, file_name)
