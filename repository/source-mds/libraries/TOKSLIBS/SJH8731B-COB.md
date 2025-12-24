# SJH8731B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJH8731B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2007/05/17                        *
*    作成者／更新者　　　：　松野　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＪＥＤＩＣＯＳにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
*    作成日／更新日　　　：　2017/03/07                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＤＣＭカーマ東日本出荷対応　　　　*
*    作成日／更新日　　　：　2017/09/04                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　受領返品取込ＤＴ作成（機能追加）　*
*    作成日／更新日　　　：　2018/02/19                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ケーヨー追加　　　　　　　　　　　*
*    作成日／更新日　　　：　2018/06/14                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注数セット変更（／１００）　　　*
*    作成日／更新日　　　：　2019/09/17                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　消費税軽減税率対応　　　　　　　　*
*    作成日／更新日　　　：　2021/02/19                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　仕入先統合対応　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH8731B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   JEDICOS   ASSIGN    TO        DA-01-S-JEDICOS
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ８８３　　　（ホーマック資材　北海道分）
     SELECT   JCAFILEA   ASSIGN    TO        DA-01-S-JCAFILEA
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAA-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ８８２　　　（ホーマック資材　仙台分）
     SELECT   JCAFILEB   ASSIGN    TO        DA-01-S-JCAFILEB
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAB-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ８８０　　　（ホーマック資材　関東分）
     SELECT   JCAFILEC   ASSIGN    TO        DA-01-S-JCAFILEC
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAC-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１３９３８　（カーマ資材）
     SELECT   JCAKAMAA   ASSIGN    TO        DA-01-S-JCAKAMAA
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHMA-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１７１３７　（カーマ植物）
     SELECT   JCAKAMAB   ASSIGN    TO        DA-01-S-JCAKAMAB
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHMB-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１３９３８１（カーマ資材２：東日本くろがねや分）
     SELECT   JCAKAMAC   ASSIGN    TO        DA-01-S-JCAKAMAC
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHMC-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１７１３７１（カーマ植物２：東日本くろがねや分）
     SELECT   JCAKAMAD   ASSIGN    TO        DA-01-S-JCAKAMAD
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KHMD-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１４２７３　（ホーマック植物　北海道分）
     SELECT   JCAFILEE   ASSIGN    TO        DA-01-S-JCAFILEE
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAE-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１４２７２　（ホーマック植物　仙台分）
     SELECT   JCAFILEF   ASSIGN    TO        DA-01-S-JCAFILEF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAF-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１４２７　　（ホーマック植物　関東分）
     SELECT   JCAFILEG   ASSIGN    TO        DA-01-S-JCAFILEG
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAG-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１００４０３（ダイキ資材）
     SELECT   JCADAIK1   ASSIGN    TO        DA-01-S-JCADAIK1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１００４４１（ダイキ植物）
     SELECT   JCADAIK2   ASSIGN    TO        DA-01-S-JCADAIK2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１００４２７（ダイキ植物）
     SELECT   JCADAIK3   ASSIGN    TO        DA-01-S-JCADAIK3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１００４０４（ダイキ九州資材）
     SELECT   JCADAIK4   ASSIGN    TO        DA-01-S-JCADAIK4
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１００４４２（ダイキ九州植物）
     SELECT   JCADAIK5   ASSIGN    TO        DA-01-S-JCADAIK5
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI5-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡ１００４２８（ダイキ九州植物）
     SELECT   JCADAIK6   ASSIGN    TO        DA-01-S-JCADAIK6
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DAI6-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ケーヨー資材　東日本 2018/02/19 NAV ST
     SELECT   DCMKEIY1   ASSIGN    TO        DA-01-S-DCMKEIY1
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI1-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ケーヨー資材　西日本 2018/02/19 NAV ST
     SELECT   DCMKEIY2   ASSIGN    TO        DA-01-S-DCMKEIY2
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI2-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ケーヨー植物　東日本 2018/02/19 NAV ST
     SELECT   DCMKEIY3   ASSIGN    TO        DA-01-S-DCMKEIY3
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI3-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ケーヨー植物　西日本 2018/02/19 NAV ST
     SELECT   DCMKEIY4   ASSIGN    TO        DA-01-S-DCMKEIY4
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    KEI4-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*ＪＣＡフォーマット（対象外）
     SELECT   JCAFILEH   ASSIGN    TO        DA-01-S-JCAFILEH
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    JCAH-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*店舗マスタ
     SELECT   TENMS1     ASSIGN    TO        DA-01-VI-TENMS1
                         ORGANIZATION        IS   INDEXED
                         ACCESS    MODE      IS   RANDOM
                         RECORD    KEY       IS   TEN-F52 TEN-F011
                         FILE      STATUS    IS   TEN-STATUS.
*#2017/09/04 NAV ST
*ＤＣＭ受領データ
     SELECT   DCMJYRW1   ASSIGN    TO        DA-01-VI-DCMJYRW1
                         ORGANIZATION        IS   INDEXED
                         ACCESS    MODE      IS   RANDOM
                         RECORD    KEY       IS   JYR-F01 JYR-F05
                                                  JYR-F06 JYR-F02
                                                  JYR-F04 JYR-F11
                         FILE      STATUS    IS   JYR-STATUS.
*#2017/09/04 NAV ED
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　３１４９  ＢＦ＝　１
******************************************************************
 FD  JEDICOS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(3147).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材　北海道）
******************************************************************
 FD  JCAFILEA            LABEL RECORD   IS   STANDARD.
*
 01  JCAA-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材　東北）
******************************************************************
 FD  JCAFILEB            LABEL RECORD   IS   STANDARD.
*
 01  JCAB-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック資材　関東）
******************************************************************
 FD  JCAFILEC            LABEL RECORD   IS   STANDARD.
*
 01  JCAC-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ資材）
******************************************************************
 FD  JCAKAMAA            LABEL RECORD   IS   STANDARD.
*
 01  KHMA-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ植物）
******************************************************************
 FD  JCAKAMAB            LABEL RECORD   IS   STANDARD.
*
 01  KHMB-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ資材２）
******************************************************************
 FD  JCAKAMAC            LABEL RECORD   IS   STANDARD.
*
 01  KHMC-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（カーマ植物２）
******************************************************************
 FD  JCAKAMAD            LABEL RECORD   IS   STANDARD.
*
 01  KHMD-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物　北海道）
******************************************************************
 FD  JCAFILEE            LABEL RECORD   IS   STANDARD.
*
 01  JCAE-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物　東北）
******************************************************************
 FD  JCAFILEF            LABEL RECORD   IS   STANDARD.
*
 01  JCAF-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ホーマック植物　関東）
******************************************************************
 FD  JCAFILEG            LABEL RECORD   IS   STANDARD.
*
 01  JCAG-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ資材）
******************************************************************
 FD  JCADAIK1            LABEL RECORD   IS   STANDARD.
*
 01  DAI1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ植物）
******************************************************************
 FD  JCADAIK2            LABEL RECORD   IS   STANDARD.
*
 01  DAI2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ植物）
******************************************************************
 FD  JCADAIK3            LABEL RECORD   IS   STANDARD.
*
 01  DAI3-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ資材）
******************************************************************
 FD  JCADAIK4            LABEL RECORD   IS   STANDARD.
*
 01  DAI4-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ植物）
******************************************************************
 FD  JCADAIK5            LABEL RECORD   IS   STANDARD.
*
 01  DAI5-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＪＣＡフォーマットファイル（ダイキ植物）
******************************************************************
 FD  JCADAIK6            LABEL RECORD   IS   STANDARD.
*
 01  DAI6-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ケーヨー資材　東日本 2018/02/19 NAV ST
******************************************************************
 FD  DCMKEIY1            LABEL RECORD   IS   STANDARD.
*
 01  KEI1-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ケーヨー資材　西日本 2018/02/19 NAV ST
******************************************************************
 FD  DCMKEIY2            LABEL RECORD   IS   STANDARD.
*
 01  KEI2-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ケーヨー植物　東日本 2018/02/19 NAV ST
******************************************************************
 FD  DCMKEIY3            LABEL RECORD   IS   STANDARD.
*
 01  KEI3-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ケーヨー植物　西日本 2018/02/19 NAV ST
******************************************************************
 FD  DCMKEIY4            LABEL RECORD   IS   STANDARD.
*
 01  KEI4-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ＪＣＡフォーマットファイル（対象外）
******************************************************************
 FD  JCAFILEH            LABEL RECORD   IS   STANDARD.
*
 01  JCAH-REC.
     03  FILLER                   PIC X(128).
******************************************************************
*    ＤＣＭ受領データ
******************************************************************
 FD  DCMJYRW1.
     COPY     DCMJYRW1  OF        XFDLIB
              JOINING   JYR       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   DJJYRHED OF XFDLIB  JOINING   HED  AS   PREFIX.
*ヘッダ情報格納領域
     COPY   DJJYRMEI OF XFDLIB  JOINING   MEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                 PIC  9(01)     VALUE  ZERO.
 01  IDX                    PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                 PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTA               PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTB               PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTC               PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMAA           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMAB           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMAC           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKAMAD           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTE               PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTF               PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTG               PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK1           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK2           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK3           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK4           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK5           PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTDAIK6           PIC  9(08)     VALUE  ZERO.
*#2018/02/19 NAV ST
 01  JCA-CNTKEIYO1          PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKEIYO2          PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKEIYO3          PIC  9(08)     VALUE  ZERO.
 01  JCA-CNTKEIYO4          PIC  9(08)     VALUE  ZERO.
*#2018/02/19 NAV ED
 01  JCA-CNTH               PIC  9(08)     VALUE  ZERO.
 01  JYR-CNT                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HTENMS-CHK-FLG         PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG       PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81             PIC  X(02)     VALUE  SPACE.
 01  WK-HED-F17             PIC  9(02)     VALUE  ZERO.
 01  WK-HED-F304            PIC  X(06)     VALUE  SPACE.
 01  WK-HED-F304-R          REDEFINES      WK-HED-F304.
     03  WK-HED-F304-H      PIC  9(06).
*#2017/09/04 NAV ST
 01  WK-JYR-TOKCD           PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO               PIC  X(07)     VALUE  SPACE.
 01  WK-HED-F304-R          REDEFINES      WK-DENNO.
     03  WK-DENNO1          PIC  9(07).
 01  WK-DEPD05-R            PIC  9(07)V99  VALUE  ZERO.
*#2017/09/04 NAV ED
*
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).   *>H
     03  WK-DEPB02          PIC  9(01).   *>0
     03  WK-DEPB03          PIC  X(07).   *>伝票番号７桁
     03  WK-DEPB04          PIC  9(06).   *>発注日
     03  WK-DEPB05          PIC  9(06).   *>納品指定日
     03  WK-DEPB06.
         05  WK-DEPB061     PIC  9(06).   *>取引先CD
         05  WK-DEPB062     PIC  9(02).   *>0
     03  WK-DEPB07.
         05  WK-DEPB071     PIC  X(20).   *>取引先名カナ
         05  WK-DEPB072     PIC  X(20).   *>0
     03  WK-DEPB08.
         05  WK-DEPB081     PIC  9(04).   *>納品先CD
         05  WK-DEPB082     PIC  X(02).
     03  WK-DEPB09          PIC  X(15).   *>納品先名カナ
     03  WK-DEPB10.
         05  WK-DEPB101     PIC  9(03).   *>部門CD
         05  WK-DEPB102     PIC  X(01).   *>空白
     03  WK-DEPB11          PIC  9(02).   *>伝票区分
     03  WK-DEPB12          PIC  9(04).   *>空白
     03  WK-DEPB13          PIC  9(02).   *>空白
     03  WK-DEPB14          PIC  9(01).   *>空白
     03  WK-DEPB15          PIC  9(01).   *>納品区分
*#2019/09/17 NAV ST 消費税軽減税率対応
*****03  WK-DEPB16          PIC  X(14).   *>空白
     03  WK-DEPB151         PIC  9(05).   *>税率
     03  WK-DEPB16          PIC  X(09).   *>空白
*#2019/09/17 NAV ED 消費税軽減税率対応
     03  WK-DEPB161         PIC  9(09).   *>伝票番号
     03  WK-DEPB17          PIC  X(01).   *>空白

*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).   *>L
     03  WK-DEPD02          PIC  9(01).   *>行
     03  WK-DEPD03.
         05  WK-DEPD031     PIC  9(07).   *>商品CD
         05  WK-DEPD032     PIC  X(01).   *>空白
     03  WK-DEPD04.
         05  WK-DEPD041     PIC  X(20).   *>商品名カナ
         05  WK-DEPD042     PIC  X(20).   *>商品名カナ
     03  WK-DEPD05          PIC  9(05)V9. *>発注数量
     03  WK-DEPD051         PIC  9(05)V9. *>納品数量
     03  WK-DEPD06          PIC  9(06)V99. *>原価単価
     03  WK-DEPD07          PIC  9(08).   *>原価金額
     03  WK-DEPD08          PIC  9(06).   *>売価単価
     03  WK-DEPD09          PIC  9(08).   *>売価金額
     03  WK-DEPD10          PIC  X(13).   *>JANCD
     03  WK-DEPD11          PIC  X(23).   *>空白
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).   *>T
     03  WK-DEPT02          PIC  9(01).   *>0
     03  WK-DEPT03          PIC  9(08).   *>原価金額
     03  WK-DEPT04          PIC  9(08).   *>売価金額
     03  WK-DEPT05          PIC  X(110).  *>空白
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  EDI-STATUS        PIC  X(02).
     03  JCAA-STATUS       PIC  X(02).
     03  JCAB-STATUS       PIC  X(02).
     03  JCAC-STATUS       PIC  X(02).
     03  KHMA-STATUS       PIC  X(02).
     03  KHMB-STATUS       PIC  X(02).
     03  KHMC-STATUS       PIC  X(02).
     03  KHMD-STATUS       PIC  X(02).
     03  JCAE-STATUS       PIC  X(02).
     03  JCAF-STATUS       PIC  X(02).
     03  JCAG-STATUS       PIC  X(02).
     03  DAI1-STATUS       PIC  X(02).
     03  DAI2-STATUS       PIC  X(02).
     03  DAI3-STATUS       PIC  X(02).
     03  DAI4-STATUS       PIC  X(02).
     03  DAI5-STATUS       PIC  X(02).
     03  DAI6-STATUS       PIC  X(02).
*#2018/02/19 NAV ST
     03  KEI1-STATUS       PIC  X(02).
     03  KEI2-STATUS       PIC  X(02).
     03  KEI3-STATUS       PIC  X(02).
     03  KEI4-STATUS       PIC  X(02).
*#2018/02/19 NAV ED
     03  JCAH-STATUS       PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  JYR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH8731B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8731B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH8731B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JEDICOS.
     MOVE      "JEDICOS "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEA.
     MOVE      "JCAFILEA"    TO   AB-FILE.
     MOVE      JCAA-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEB.
     MOVE      "JCAFILEB"    TO   AB-FILE.
     MOVE      JCAB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEC.
     MOVE      "JCAFILEC"    TO   AB-FILE.
     MOVE      JCAC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMAA.
     MOVE      "JCAKAMAA"    TO   AB-FILE.
     MOVE      KHMA-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEE.
     MOVE      "JCAFILEE"    TO   AB-FILE.
     MOVE      JCAE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEF.
     MOVE      "JCAFILEF"    TO   AB-FILE.
     MOVE      JCAF-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEG.
     MOVE      "JCAFILEG"    TO   AB-FILE.
     MOVE      JCAG-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMAB.
     MOVE      "JCAKAMAB"    TO   AB-FILE.
     MOVE      KHMB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK1.
     MOVE      "JCADAIK1"    TO   AB-FILE.
     MOVE      DAI1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK2.
     MOVE      "JCADAIK2"    TO   AB-FILE.
     MOVE      DAI2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK3.
     MOVE      "JCADAIK3"    TO   AB-FILE.
     MOVE      DAI3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK4.
     MOVE      "JCADAIK4"    TO   AB-FILE.
     MOVE      DAI4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC14          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK5.
     MOVE      "JCADAIK5"    TO   AB-FILE.
     MOVE      DAI5-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC15          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCADAIK6.
     MOVE      "JCADAIK6"    TO   AB-FILE.
     MOVE      DAI6-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC16          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAFILEH.
     MOVE      "JCAFILEH"    TO   AB-FILE.
     MOVE      JCAH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC17          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "    TO   AB-FILE.
     MOVE      TEN-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC18          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMAC.
     MOVE      "JCAKAMAC"    TO   AB-FILE.
     MOVE      KHMC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC19          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JCAKAMAD.
     MOVE      "JCAKAMAD"    TO   AB-FILE.
     MOVE      KHMD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC20          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMKEIY1.
     MOVE      "DCMKEIY1"    TO   AB-FILE.
     MOVE      KEI1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC21          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMKEIY2.
     MOVE      "DCMKEIY2"    TO   AB-FILE.
     MOVE      KEI2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC22          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMKEIY3.
     MOVE      "DCMKEIY3"    TO   AB-FILE.
     MOVE      KEI3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC23          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMKEIY4.
     MOVE      "DCMKEIY4"    TO   AB-FILE.
     MOVE      KEI4-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC24          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMJYRW1.
     MOVE      "DCMJYRW1"    TO   AB-FILE.
     MOVE      JYR-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY   "JYR-F01 = " JYR-F01 UPON CONS.
     DISPLAY   "JYR-F05 = " JYR-F05 UPON CONS.
     DISPLAY   "JYR-F06 = " JYR-F06 UPON CONS.
     DISPLAY   "JYR-F02 = " JYR-F02 UPON CONS.
     DISPLAY   "JYR-F11 = " JYR-F11 UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JEDICOS   TENMS1.
     OPEN     OUTPUT    JCAFILEA  JCAFILEB  JCAFILEC  JCAKAMAA.
     OPEN     OUTPUT    JCAFILEE  JCAFILEF  JCAFILEG  JCAKAMAB.
     OPEN     OUTPUT    JCADAIK1  JCADAIK2  JCADAIK3  JCAFILEH.
     OPEN     OUTPUT    JCADAIK4  JCADAIK5  JCADAIK6.
     OPEN     OUTPUT    JCAKAMAC  JCAKAMAD.
*****#2017/09/04 NAV ST
     OPEN     OUTPUT    DCMJYRW1.
*****#2017/09/04 NAV ED
*#2018/02/19 NAV ST
     OPEN     OUTPUT    DCMKEIY1  DCMKEIY2  DCMKEIY3  DCMKEIY4.
*#2018/02/19 NAV ED
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT.
     MOVE     ZERO      TO        JCA-CNTA JCA-CNTB
                                  JCA-CNTC JCA-CNTKAMAA
                                  JCA-CNTE JCA-CNTF JCA-CNTG
                                  JCA-CNTKAMAB JCA-CNTDAIK1
                                  JCA-CNTDAIK2 JCA-CNTH
                     JCA-CNTDAIK4 JCA-CNTDAIK5 JCA-CNTDAIK6
                                  JCA-CNTKAMAC JCA-CNTKAMAD
*#2018/02/19 NAV ST
      JCA-CNTKEIYO1 JCA-CNTKEIYO2 JCA-CNTKEIYO3 JCA-CNTKEIYO4.
*#2018/02/19 NAV ED
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
     MOVE     SPACE     TO        WK-DEPT-REC.
     INITIALIZE                   WK-DEPT-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*
     PERFORM  JEDICOS-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
***********ワークエリア初期化
           MOVE      SPACE       TO   HED-REC
           INITIALIZE                 HED-REC
***********ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO   HED-REC
***********JCA FORMAT 初期化
           MOVE      SPACE       TO   WK-DEPB-REC
           INITIALIZE                 WK-DEPB-REC
           MOVE      "H"         TO   WK-DEPB01
           MOVE      ZERO        TO   WK-DEPB02
*発注伝票番号
           MOVE      HED-F03     TO   WK-DEPB03  WK-DEPB161
****
******#2021/02/19 NAV ST
        IF  HED-F03(8:2) NOT NUMERIC
******#2021/02/19 NAV ED
          MOVE   HED-F03(1:7)    TO   WK-DENNO
          MOVE   WK-DENNO1       TO   WK-DEPB03  WK-DEPB161
******#2021/02/19 NAV  ST
        ELSE
          MOVE   HED-F03(3:7)    TO   WK-DEPB03
          MOVE   HED-F03         TO   WK-DEPB161
        END-IF
*発注日
           MOVE      HED-F05     TO   WK-DEPB04
*納品指定日
           MOVE      HED-F06     TO   WK-DEPB05
*取引先ＣＤ
           MOVE      HED-F27     TO   WK-DEPB061
           MOVE      ZERO        TO   WK-DEPB062
*取引先名称カナ
           MOVE      HED-F29     TO   WK-DEPB07
*納品先コード
           MOVE      HED-F21     TO   WK-DEPB081
           MOVE      SPACE       TO   WK-DEPB082
*納品先名カナ
           MOVE      HED-F22     TO   WK-DEPB09
*部門ＣＤ
           MOVE      HED-F14     TO   WK-DEPB101
           MOVE      SPACE       TO   WK-DEPB102
***********MOVE      ZERO        TO   WK-DEPB11
*伝票区分
           MOVE      HED-F081    TO   WK-DEPB11
           MOVE      ZERO        TO   WK-DEPB12
           MOVE      ZERO        TO   WK-DEPB13
           MOVE      ZERO        TO   WK-DEPB14
*納品区分
           MOVE      HED-F10     TO   WK-DEPB15
           MOVE      SPACE       TO   WK-DEPB16
*#2019/09/17 NAV ST 消費税軽減税率対応
           MOVE      HED-F32     TO   WK-DEPB151
*#2019/09/17 NAV ED 消費税軽減税率対応
           MOVE      SPACE       TO   WK-DEPB17
*発注事業部コード
           MOVE      HED-F17     TO   WK-HED-F17
***************2021/03/23 NAV ST
           MOVE      HED-F304    TO   WK-HED-F304
           IF  WK-HED-F304-H = 880 OR 882 OR 883
           OR  1427 OR 14272 OR 14273
             IF  HED-F064  <  20210301
                 IF  WK-HED-F17  =  2
                     MOVE  1       TO   WK-HED-F17
                 END-IF
                 IF  WK-HED-F17  =  3
                     MOVE  2       TO   WK-HED-F17
                 END-IF
                 IF  WK-HED-F17  =  4
                     MOVE  3       TO   WK-HED-F17
                 END-IF
             END-IF
           END-IF
***************2021/03/23 NAV ED
*個別取引先コード
           MOVE      HED-F304    TO   WK-HED-F304
           MOVE      SPACE       TO   TEN-REC
           INITIALIZE                 TEN-REC
           MOVE    WK-HED-F304-H TO   TEN-F52
           MOVE      HED-F21     TO   TEN-F011
           PERFORM   HTENMS-READ-SEC
***********取引先別に処理を振り分けます。
           EVALUATE  WK-HED-F304
***************ホーマック資材
               WHEN  000880
               WHEN  000882
               WHEN  000883
****************納品日
               MOVE      HED-F063    TO   WK-DEPB05
               EVALUATE  WK-HED-F17
***************#2021/03/08 NAV ST
***************#   WHEN  2
                   WHEN  1
***************#2021/03/08 NAV ED
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   883         TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO JCAA-REC
                         WRITE  JCAA-REC
                         ADD    1    TO   JCA-CNTA
***************#2021/03/08 NAV ST
***************#   WHEN  3
                   WHEN  2
***************#2021/03/08 NAV ED
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   882         TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO JCAB-REC
                         WRITE  JCAB-REC
                         ADD    1    TO   JCA-CNTB
***************#2021/03/08 NAV ST
***************#   WHEN  4
                   WHEN  3
***************#2021/03/08 NAV ED
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   880         TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO JCAC-REC
                         WRITE  JCAC-REC
                         ADD    1    TO   JCA-CNTC
                   WHEN  OTHER
                         MOVE   WK-DEPB-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************ホーマック植物
               WHEN  001427
               WHEN  014272
               WHEN  014273
****************納品日
               MOVE      HED-F063    TO   WK-DEPB05
               EVALUATE  WK-HED-F17
***************#2021/03/08 NAV ST
***************#   WHEN  2
                   WHEN  1
***************#2021/03/08 NAV ED
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   14273       TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO JCAE-REC
                         WRITE  JCAE-REC
                         ADD    1    TO   JCA-CNTE
***************#2021/03/08 NAV ST
***************#   WHEN  3
                   WHEN  2
***************#2021/03/08 NAV ED
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   14272       TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO JCAF-REC
                         WRITE  JCAF-REC
                         ADD    1    TO   JCA-CNTF
***************#2021/03/08 NAV ST
***************#   WHEN  4
                   WHEN  3
***************#2021/03/08 NAV ED
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   1427        TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO JCAG-REC
                         WRITE  JCAG-REC
                         ADD    1    TO   JCA-CNTG
                   WHEN  OTHER
                         MOVE   WK-DEPB-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************カーマ資材
               WHEN  013938
*********************伝票番号の下７桁セット
                     MOVE   HED-F03(3:7)   TO   WK-DEPB03
*********************MOVE   WK-DEPB-REC TO KHMA-REC
*********************WRITE  KHMA-REC
*********************ADD    1    TO   JCA-CNTKAMAA
*********************○１３９３８：カーマ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   13938       TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO KHMA-REC
                         WRITE  KHMA-REC
                         ADD    1    TO   JCA-CNTKAMAA
*********************○１３９３８１：カーマ東日本（資材）
                     ELSE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   139381      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO KHMC-REC
                         WRITE  KHMC-REC
                         ADD    1    TO   JCA-CNTKAMAC
                     END-IF
***************カーマ植物
               WHEN  017137
*********************伝票番号の下７桁セット
                     MOVE   HED-F03(3:7)   TO   WK-DEPB03
*********************MOVE   WK-DEPB-REC TO KHMB-REC
*********************WRITE  KHMB-REC
*********************ADD    1    TO   JCA-CNTKAMAB
*********************○１７１３７：カーマ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   17137       TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO KHMB-REC
                         WRITE  KHMB-REC
                         ADD    1    TO   JCA-CNTKAMAB
*********************○１７１３７１：カーマ東日本（資材）
                     ELSE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   171371      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO KHMD-REC
                         WRITE  KHMD-REC
                         ADD    1    TO   JCA-CNTKAMAD
                     END-IF
***************ダイキ
***************WHEN  100403   WHEN  100441  WHEN   100434
               WHEN  100403
*********************MOVE   WK-DEPB-REC TO DAI1-REC
*                    WRITE  DAI1-REC
*********************ADD    1    TO   JCA-CNTDAIK1
*********************○１００４０３：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   100403      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO DAI1-REC
                         WRITE  DAI1-REC
                         ADD    1    TO   JCA-CNTDAIK1
*********************○１００４０４：ダイキ九州　（資材）
                     ELSE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   100404      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO DAI4-REC
                         WRITE  DAI4-REC
                         ADD    1    TO   JCA-CNTDAIK4
                     END-IF
***************ダイキ
***************WHEN  100427
               WHEN  100441
*********************MOVE   WK-DEPB-REC TO DAI2-REC
*                    WRITE  DAI2-REC
*********************ADD    1    TO   JCA-CNTDAIK2
*********************○１００４４１：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   100441      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO DAI2-REC
                         WRITE  DAI2-REC
                         ADD    1    TO   JCA-CNTDAIK2
*********************○１００４４２：ダイキ九州　（資材）
                     ELSE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   100442      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO DAI5-REC
                         WRITE  DAI5-REC
                         ADD    1    TO   JCA-CNTDAIK5
                     END-IF
***************ダイキ
               WHEN  100427
*********************MOVE   WK-DEPB-REC TO DAI3-REC
*                    WRITE  DAI3-REC
*********************ADD    1    TO   JCA-CNTDAIK3
*********************○１００４２７：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   100427      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO DAI3-REC
                         WRITE  DAI3-REC
                         ADD    1    TO   JCA-CNTDAIK3
*********************○１００４２８：ダイキ九州　（植物）
                     ELSE
*#2017/09/04 NAV ST
*************************自社取引先ＣＤセット
                         MOVE   100428      TO  WK-JYR-TOKCD
*#2017/09/04 NAV ED
                         MOVE   WK-DEPB-REC TO DAI6-REC
                         WRITE  DAI6-REC
                         ADD    1    TO   JCA-CNTDAIK6
                     END-IF
*#2018/02/20 NAV ST ********************************
***************ケーヨー資材
               WHEN  000173
*********************◯ケーヨーの場合、店舗Ｍ再度読込
                     MOVE      1731        TO   TEN-F52
                     MOVE      HED-F21     TO   TEN-F011
                     PERFORM   HTENMS-READ-SEC
*********************○１７３１：ケーヨー資材東
                     IF  HTENMS-CHK-FLG = SPACE
*************************自社取引先ＣＤセット
                         MOVE   1731        TO  WK-JYR-TOKCD
                         MOVE   WK-DEPB-REC TO KEI1-REC
                         WRITE  KEI1-REC
                         ADD    1    TO   JCA-CNTKEIYO1
*********************○１７３２：ケーヨー植物西
                     ELSE
*************************自社取引先ＣＤセット
                         MOVE   1732        TO  WK-JYR-TOKCD
                         MOVE   WK-DEPB-REC TO KEI2-REC
                         WRITE  KEI2-REC
                         ADD    1    TO   JCA-CNTKEIYO2
                     END-IF
***************ケーヨー植物
               WHEN  000760
*********************◯ケーヨーの場合、店舗Ｍ再度読込
                     MOVE      7601        TO   TEN-F52
                     MOVE      HED-F21     TO   TEN-F011
                     PERFORM   HTENMS-READ-SEC
*********************○７６０１：ケーヨー植物東
                     IF  HTENMS-CHK-FLG = SPACE
*************************自社取引先ＣＤセット
                         MOVE   7601        TO  WK-JYR-TOKCD
                         MOVE   WK-DEPB-REC TO KEI3-REC
                         WRITE  KEI3-REC
                         ADD    1    TO   JCA-CNTKEIYO3
*********************○７６０２：ケーヨー植物西
                     ELSE
                         MOVE   7602        TO  WK-JYR-TOKCD
                         MOVE   WK-DEPB-REC TO KEI4-REC
                         WRITE  KEI4-REC
                         ADD    1    TO   JCA-CNTKEIYO4
                     END-IF
*#2018/02/20 NAV ED ********************************
***************対象外
               WHEN  OTHER
                     MOVE   WK-DEPB-REC TO JCAH-REC
                     WRITE  JCAH-REC
                     ADD    1    TO   JCA-CNTH
           END-EVALUATE

     END-IF.
*明細行
     IF    EDI-01  =  "DT"
***********ＪＣＡフォーマット初期化(明細）
           MOVE      SPACE       TO   WK-DEPD-REC
           INITIALIZE                 WK-DEPD-REC
***********ワークエリア初期化
           MOVE      SPACE       TO   MEI-REC
           INITIALIZE                 MEI-REC
***********ヘッダ情報→ワークにセット
           MOVE      EDI-REC     TO   MEI-REC
***********ＪＣＡフォーマット項目セット
           MOVE      "L"         TO   WK-DEPD01
***********MOVE      1           TO   WK-DEPD02
*行番号
           MOVE      MEI-F03     TO   WK-DEPD02
*商品コード
           MOVE      MEI-F04     TO   WK-DEPD031
           MOVE      SPACE       TO   WK-DEPD032
*商品名カナ
           MOVE      MEI-F07     TO   WK-DEPD04
*発注数量
*#2018/06/14 NAV ST 発注数／１００（桁数合わせをＷＫで実施）
********   MOVE      MEI-F10     TO   WK-DEPD05
           COMPUTE WK-DEPD05-R = MEI-F10 / 100
           MOVE      WK-DEPD05-R TO   WK-DEPD05
************F  WK-JYR-TOKCD  =  1731
*          AND WK-DEPB161    =  6531297
*              DISPLAY "MEI-F10  = " MEI-F10  UPON CONS
*              DISPLAY "MEI-F122 = " MEI-F122 UPON CONS
*              DISPLAY "WK-DEPD05-R = " WK-DEPD05-R UPON CONS
*              DISPLAY "WK-DEPD05   = " WK-DEPD05   UPON CONS
***********END-IF
*#2018/06/14 NAV ED 発注数／１００（桁数合わせをＷＫで実施）
*納品数
           MOVE      MEI-F122    TO   WK-DEPD051
*原価単価／１００
           COMPUTE WK-DEPD06 = MEI-F15 / 100
*発注原価金額
           MOVE      MEI-F13     TO   WK-DEPD07
*売価単価
           MOVE      MEI-F16     TO   WK-DEPD08
*発注売価金額
           MOVE      MEI-F14     TO   WK-DEPD09
*ＪＡＮＣＤ
           MOVE      MEI-F02     TO   WK-DEPD10
           MOVE      SPACE       TO   WK-DEPD11
*納品書伝票番号
           MOVE      MEI-F051    TO   WK-DEPD11
***********取引先別に処理を振り分けます。
           EVALUATE  WK-HED-F304
***************ホーマック資材
               WHEN  000880
               WHEN  000882
               WHEN  000883
               EVALUATE  WK-HED-F17
***************#2021/03/08 NAV ST
***************#   WHEN  2
                   WHEN  1
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPD-REC TO JCAA-REC
                         WRITE  JCAA-REC
                         ADD    1    TO   JCA-CNTA
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
***************#2021/03/08 NAV ST
***************#   WHEN  3
                   WHEN  2
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPD-REC TO JCAB-REC
                         WRITE  JCAB-REC
                         ADD    1    TO   JCA-CNTB
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
***************#2021/03/08 NAV ST
***************#   WHEN  4
                   WHEN  3
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPD-REC TO JCAC-REC
                         WRITE  JCAC-REC
                         ADD    1    TO   JCA-CNTC
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                   WHEN  OTHER
                         MOVE   WK-DEPD-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************ホーマック植物
               WHEN  001427
               WHEN  014272
               WHEN  014273
               EVALUATE  WK-HED-F17
***************#2021/03/08 NAV ST
***************#   WHEN  2
                   WHEN  1
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPD-REC TO JCAE-REC
                         WRITE  JCAE-REC
                         ADD    1    TO   JCA-CNTE
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
***************#2021/03/08 NAV ST
***************#   WHEN  3
                   WHEN  2
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPD-REC TO JCAF-REC
                         WRITE  JCAF-REC
                         ADD    1    TO   JCA-CNTF
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
***************#2021/03/08 NAV ST
***************#   WHEN  4
                   WHEN  3
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPD-REC TO JCAG-REC
                         WRITE  JCAG-REC
                         ADD    1    TO   JCA-CNTG
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                   WHEN  OTHER
                         MOVE   WK-DEPD-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************カーマ資材
               WHEN  013938
*********************MOVE   WK-DEPD-REC TO KHMA-REC
*********************WRITE  KHMA-REC
*********************ADD    1    TO   JCA-CNTKAMAA
*********************○１３９３８：カーマ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO KHMA-REC
                         WRITE  KHMA-REC
                         ADD    1    TO   JCA-CNTKAMAA
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
*********************○１３９３８１：カーマ東日本（資材）
                     ELSE
                         MOVE   WK-DEPD-REC TO KHMC-REC
                         WRITE  KHMC-REC
                         ADD    1    TO   JCA-CNTKAMAC
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                     END-IF
***************カーマ植物
               WHEN  017137
*********************MOVE   WK-DEPD-REC TO KHMB-REC
*********************WRITE  KHMB-REC
*********************ADD    1    TO   JCA-CNTKAMAB
*********************○１７１３７：カーマ西日本（植物）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO KHMB-REC
                         WRITE  KHMB-REC
                         ADD    1    TO   JCA-CNTKAMAB
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
*********************○１７１３７１：カーマ東日本（植物）
                     ELSE
                         MOVE   WK-DEPD-REC TO KHMD-REC
                         WRITE  KHMD-REC
                         ADD    1    TO   JCA-CNTKAMAD
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                     END-IF
***************ダイキ
***************WHEN  100403  WHEN  100441  WHEN  100434
               WHEN  100403
*********************○１００４０３：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO DAI1-REC
                         WRITE  DAI1-REC
                         ADD    1    TO   JCA-CNTDAIK1
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
*********************○１００４０４：ダイキ九州　（資材）
                     ELSE
                         MOVE   WK-DEPD-REC TO DAI4-REC
                         WRITE  DAI4-REC
                         ADD    1    TO   JCA-CNTDAIK4
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                     END-IF
***************ダイキ
***************WHEN  100427
               WHEN  100441
******************** MOVE   WK-DEPD-REC TO DAI2-REC
*                    WRITE  DAI2-REC
*********************ADD    1    TO   JCA-CNTDAIK2
*********************○１００４４１：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO DAI2-REC
                         WRITE  DAI2-REC
                         ADD    1    TO   JCA-CNTDAIK2
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
*********************○１００４４２：ダイキ九州　（資材）
                     ELSE
                         MOVE   WK-DEPD-REC TO DAI5-REC
                         WRITE  DAI5-REC
                         ADD    1    TO   JCA-CNTDAIK5
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                     END-IF
***************ダイキ
               WHEN  100427
*********************MOVE   WK-DEPD-REC TO DAI3-REC
*                    WRITE  DAI3-REC
*********************ADD    1    TO   JCA-CNTDAIK3
*********************○１００４２７：ダイキ西日本（植物）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO DAI3-REC
                         WRITE  DAI3-REC
                         ADD    1    TO   JCA-CNTDAIK3
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
*********************○１００４２８：ダイキ九州　（植物）
                     ELSE
                         MOVE   WK-DEPD-REC TO DAI6-REC
                         WRITE  DAI6-REC
                         ADD    1    TO   JCA-CNTDAIK6
*2017/09/04 NAV ST
                         PERFORM  DCMJYRW1-SET-SEC
*2017/09/04 NAV ED
                     END-IF
*#2018/02/20 NAV ST *********************************
***************ケーヨー資材
               WHEN  000173
*********************○１７３１：ケーヨー資材東
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO KEI1-REC
                         WRITE  KEI1-REC
                         ADD    1    TO   JCA-CNTKEIYO1
                         PERFORM  DCMJYRW1-SET-SEC
*********************○１７３２：ケーヨー資材西
                     ELSE
                         MOVE   WK-DEPD-REC TO KEI2-REC
                         WRITE  KEI2-REC
                         ADD    1    TO   JCA-CNTKEIYO2
                         PERFORM  DCMJYRW1-SET-SEC
                     END-IF
***************ケーヨー植物
               WHEN  000760
*********************○７６０１：ケーヨー植物東
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPD-REC TO KEI3-REC
                         WRITE  KEI3-REC
                         ADD    1    TO   JCA-CNTKEIYO3
                         PERFORM  DCMJYRW1-SET-SEC
*********************○７６０２：ケーヨー植物西
                     ELSE
                         MOVE   WK-DEPD-REC TO KEI4-REC
                         WRITE  KEI4-REC
                         ADD    1    TO   JCA-CNTKEIYO4
                         PERFORM  DCMJYRW1-SET-SEC
                     END-IF
*#2018/02/20 NAV ED *********************************
***************対象外
               WHEN  OTHER
                     MOVE   WK-DEPD-REC TO JCAH-REC
                     WRITE  JCAH-REC
                     ADD    1    TO   JCA-CNTH
           END-EVALUATE
***********ＪＣＡフォーマット初期化(トレイラー）
           MOVE      SPACE       TO   WK-DEPT-REC
           INITIALIZE                 WK-DEPT-REC
           MOVE      "T"         TO   WK-DEPT01
           MOVE      ZERO        TO   WK-DEPT02
*発注原価金額
           MOVE      MEI-F13     TO   WK-DEPT03
*発注売価金額
           MOVE      MEI-F14     TO   WK-DEPT04
           MOVE      SPACE       TO   WK-DEPT05
***********取引先別に処理を振り分けます。
           EVALUATE  WK-HED-F304
***************ホーマック資材
               WHEN  000880
               WHEN  000882
               WHEN  000883
               EVALUATE  WK-HED-F17
***************#2021/03/08 NAV ST
***************#   WHEN  2
                   WHEN  1
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPT-REC TO JCAA-REC
                         WRITE  JCAA-REC
                         ADD    1    TO   JCA-CNTA
***************#2021/03/08 NAV ST
***************#   WHEN  3
                   WHEN  2
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPT-REC TO JCAB-REC
                         WRITE  JCAB-REC
                         ADD    1    TO   JCA-CNTB
***************#2021/03/08 NAV ST
***************#   WHEN  4
                   WHEN  3
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPT-REC TO JCAC-REC
                         WRITE  JCAC-REC
                         ADD    1    TO   JCA-CNTC
                   WHEN  OTHER
                         MOVE   WK-DEPT-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************ホーマック植物
               WHEN  001427
               WHEN  014272
               WHEN  014273
               EVALUATE  WK-HED-F17
***************#2021/03/08 NAV ST
***************#   WHEN  2
                   WHEN  1
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPT-REC TO JCAE-REC
                         WRITE  JCAE-REC
                         ADD    1    TO   JCA-CNTE
***************#2021/03/08 NAV ST
***************#   WHEN  3
                   WHEN  2
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPT-REC TO JCAF-REC
                         WRITE  JCAF-REC
                         ADD    1    TO   JCA-CNTF
***************#2021/03/08 NAV ST
***************#   WHEN  4
                   WHEN  3
***************#2021/03/08 NAV ED
                         MOVE   WK-DEPT-REC TO JCAG-REC
                         WRITE  JCAG-REC
                         ADD    1    TO   JCA-CNTG
                   WHEN  OTHER
                         MOVE   WK-DEPT-REC TO JCAH-REC
                         WRITE  JCAH-REC
                         ADD    1    TO   JCA-CNTH
               END-EVALUATE
***************カーマ資材
               WHEN  013938
*********************MOVE   WK-DEPT-REC TO KHMA-REC
*********************WRITE  KHMA-REC
*********************ADD    1    TO   JCA-CNTKAMAA
*********************○１３９３８：カーマ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO KHMA-REC
                         WRITE  KHMA-REC
                         ADD    1    TO   JCA-CNTKAMAA
*********************○１３９３８１：カーマ東日本（資材）
                     ELSE
                         MOVE   WK-DEPT-REC TO KHMC-REC
                         WRITE  KHMC-REC
                         ADD    1    TO   JCA-CNTKAMAC
                     END-IF
***************カーマ植物
               WHEN  017137
*********************MOVE   WK-DEPT-REC TO KHMB-REC
*********************WRITE  KHMB-REC
*********************ADD    1    TO   JCA-CNTKAMAB
*********************○１７１３７：カーマ西日本（植物）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO KHMB-REC
                         WRITE  KHMB-REC
                         ADD    1    TO   JCA-CNTKAMAB
*********************○１７１３７１：カーマ東日本（植物）
                     ELSE
                         MOVE   WK-DEPT-REC TO KHMD-REC
                         WRITE  KHMD-REC
                         ADD    1    TO   JCA-CNTKAMAD
                     END-IF
***************ダイキ資材
***************WHEN  100403  WHEN  100441   WHEN   100434
               WHEN  100403
*********************MOVE   WK-DEPT-REC TO DAI1-REC
*                    WRITE  DAI1-REC
*********************ADD    1    TO   JCA-CNTDAIK1
*********************○１００４０３：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO DAI1-REC
                         WRITE  DAI1-REC
                         ADD    1    TO   JCA-CNTDAIK1
*********************○１００４０４：ダイキ九州　（資材）
                     ELSE
                         MOVE   WK-DEPT-REC TO DAI4-REC
                         WRITE  DAI4-REC
                         ADD    1    TO   JCA-CNTDAIK4
                     END-IF
***************ダイキ植物
***************WHEN  100427
               WHEN  100441
*********************MOVE   WK-DEPT-REC TO DAI2-REC
*                    WRITE  DAI2-REC
*********************ADD    1    TO   JCA-CNTDAIK2
*********************○１００４４１：ダイキ西日本（資材）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO DAI2-REC
                         WRITE  DAI2-REC
                         ADD    1    TO   JCA-CNTDAIK2
*********************○１００４４２：ダイキ九州　（資材）
                     ELSE
                         MOVE   WK-DEPT-REC TO DAI5-REC
                         WRITE  DAI5-REC
                         ADD    1    TO   JCA-CNTDAIK5
                     END-IF
***************ダイキ植物
               WHEN  100427
*********************MOVE   WK-DEPT-REC TO DAI3-REC
*                    WRITE  DAI3-REC
*********************ADD    1    TO   JCA-CNTDAIK3
*********************○１００４２７：ダイキ西日本（植物）
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO DAI3-REC
                         WRITE  DAI3-REC
                         ADD    1    TO   JCA-CNTDAIK3
*********************○１００４２８：ダイキ九州　（植物）
                     ELSE
                         MOVE   WK-DEPT-REC TO DAI6-REC
                         WRITE  DAI6-REC
                         ADD    1    TO   JCA-CNTDAIK6
                     END-IF
*#2018/02/20 NAV ST *********************************
***************ケーヨー資材
               WHEN  000173
*********************○１７３１：ケーヨー資材東
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO KEI1-REC
                         WRITE  KEI1-REC
                         ADD    1    TO   JCA-CNTKEIYO1
*********************○１７３２：ケーヨー資材西
                     ELSE
                         MOVE   WK-DEPT-REC TO KEI2-REC
                         WRITE  KEI2-REC
                         ADD    1    TO   JCA-CNTKEIYO2
                     END-IF
***************ケーヨー植物
               WHEN  000760
*********************○７６０１：ケーヨー植物東
                     IF  HTENMS-CHK-FLG = SPACE
                         MOVE   WK-DEPT-REC TO KEI3-REC
                         WRITE  KEI3-REC
                         ADD    1    TO   JCA-CNTKEIYO3
*********************○７６０２：ケーヨー植物西
                     ELSE
                         MOVE   WK-DEPT-REC TO KEI4-REC
                         WRITE  KEI4-REC
                         ADD    1    TO   JCA-CNTKEIYO4
                     END-IF
***************対象外
               WHEN  OTHER
                     MOVE   WK-DEPT-REC TO JCAH-REC
                     WRITE  JCAH-REC
                     ADD    1    TO   JCA-CNTH
           END-EVALUATE
     END-IF.
*
     PERFORM JEDICOS-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル読み込み　　　　　　　　　　　　　　　　*
****************************************************************
 JEDICOS-READ-SEC      SECTION.
*
     MOVE "JEDICOS-READ-SEC" TO       S-NAME.
*
     READ     JEDICOS
              AT END
              MOVE     9      TO    END-FG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 JEDICOS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 HTENMS-READ-SEC  SECTION.
*
     MOVE    SPACE         TO    HTENMS-CHK-FLG.
     READ    TENMS1
             INVALID
             MOVE  "INV"   TO    HTENMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO    HTENMS-INV-FLG
     END-READ.
*
     IF  HTENMS-INV-FLG = "INV"
         MOVE  SPACE       TO    HTENMS-CHK-FLG
     ELSE
         IF  TEN-F73 = SPACE
*************西日本ダイキ／西日本カーマ
             MOVE SPACE    TO    HTENMS-CHK-FLG
         ELSE
*************九州ダイキ（サンコー）／東日本カーマ
             MOVE "CHK"    TO    HTENMS-CHK-FLG
         END-IF
     END-IF.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　ＤＣＭ受領ワーク項目セット
****************************************************************
 DCMJYRW1-SET-SEC      SECTION.
*
*****レコード初期化
      MOVE   SPACE         TO    JYR-REC.
      INITIALIZE                 JYR-REC.
*取引先コード
      MOVE   WK-JYR-TOKCD        TO   JYR-F01.
*伝票番号
      IF  WK-JYR-TOKCD  =  880   OR   882   OR  883   OR
                           1427  OR   14272 OR  14273
******#2021/02/19 NAV ST
        IF  HED-F03(8:2) NOT NUMERIC
******#2021/02/19 NAV ED
          MOVE   HED-F03(1:7)    TO   WK-DENNO
          MOVE   WK-DENNO1       TO   JYR-F02
******#2021/02/19 NAV  ST
        ELSE
          MOVE   HED-F03         TO   JYR-F02
        END-IF
******#2021/02/19 NAV  ED
      ELSE
          MOVE   HED-F03         TO   JYR-F02
      END-IF.
*伝票種別
      MOVE   HED-F08             TO   JYR-F03.
*伝票区分
      MOVE   HED-F081            TO   JYR-F04.
*納品先ＣＤ
      MOVE   HED-F21             TO   JYR-F05.
*仕入計上日
      MOVE   HED-F064            TO   JYR-F06.
*部門ＣＤ
      MOVE   HED-F14             TO   JYR-F07.
*行番号
      MOVE   MEI-F03             TO   JYR-F11.
*相手商品ＣＤ
      MOVE   MEI-F02             TO   JYR-F12.
*納品数
      MOVE   MEI-F122            TO   JYR-F13.
*原単価
      COMPUTE  JYR-F14  =  MEI-F15  /  100.
*納品原価金額
      MOVE   MEI-F161            TO   JYR-F15.
*商品名カナ
      MOVE   MEI-F07             TO   JYR-F16.
*規格名カナ
      MOVE   MEI-F084            TO   JYR-F17.
*欠品理由
      MOVE   MEI-F19             TO   JYR-F18.
*売単価
      MOVE   MEI-F16             TO   JYR-F19.
*ＤＣＭ受領ワーク作成
      WRITE  JYR-REC.
      ADD    1                   TO   JYR-CNT.
*
 DCMJYRW1-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     JEDICOS JCAFILEA JCAFILEB JCAFILEC JCAKAMAA.
     CLOSE     JCAFILEE JCAFILEF JCAFILEG JCAKAMAB.
     CLOSE     JCADAIK1 JCADAIK2 JCADAIK3 JCAFILEH.
     CLOSE     JCADAIK4 JCADAIK5 JCADAIK6 TENMS1.
     CLOSE     JCAKAMAC JCAKAMAD.
*#2018/02/19 NAV ST
     CLOSE     DCMKEIY1 DCMKEIY2 DCMKEIY3 DCMKEIY4.
*#2018/02/19 NAV ED
*#2017/09/04 NAV ST
     CLOSE     DCMJYRW1.
*#2017/09/04 NAV END
*
     DISPLAY NC"＃ＨＣ資材　北海道ＣＮＴ＝"  JCA-CNTA
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　東北　ＣＮＴ＝"  JCA-CNTB
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ資材　関東　ＣＮＴ＝"  JCA-CNTC
                                             UPON CONS.
     DISPLAY NC"＃カーマ資材　　　ＣＮＴ＝"  JCA-CNTKAMAA
                                             UPON CONS.
     DISPLAY NC"＃カーマ植物　　　ＣＮＴ＝"  JCA-CNTKAMAB
                                             UPON CONS.
     DISPLAY NC"＃カーマ資材２　　ＣＮＴ＝"  JCA-CNTKAMAC
                                             UPON CONS.
     DISPLAY NC"＃カーマ植物２　　ＣＮＴ＝"  JCA-CNTKAMAD
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　北海道ＣＮＴ＝"  JCA-CNTE
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　東北　ＣＮＴ＝"  JCA-CNTF
                                             UPON CONS.
     DISPLAY NC"＃ＨＣ植物　関東　ＣＮＴ＝"  JCA-CNTG
                                             UPON CONS.
     DISPLAY NC"＃ダイキ資材４０３ＣＮＴ＝"  JCA-CNTDAIK1
                                             UPON CONS.
     DISPLAY NC"＃ダイキ資材４４１ＣＮＴ＝"  JCA-CNTDAIK2
                                             UPON CONS.
     DISPLAY NC"＃ダイキ植物４２７ＣＮＴ＝"  JCA-CNTDAIK3
                                             UPON CONS.
     DISPLAY NC"＃サンコ資材４０４ＣＮＴ＝"  JCA-CNTDAIK4
                                             UPON CONS.
     DISPLAY NC"＃サンコ資材４４２ＣＮＴ＝"  JCA-CNTDAIK5
                                             UPON CONS.
     DISPLAY NC"＃サンコ植物４２８ＣＮＴ＝"  JCA-CNTDAIK6
                                             UPON CONS.
*#2018/02/19 NAV ST
     DISPLAY NC"＃ケーヨー資材　東ＣＮＴ＝"  JCA-CNTKEIYO1
                                             UPON CONS.
     DISPLAY NC"＃ケーヨー資材　西ＣＮＴ＝"  JCA-CNTKEIYO2
                                             UPON CONS.
     DISPLAY NC"＃ケーヨー植物　東ＣＮＴ＝"  JCA-CNTKEIYO3
                                             UPON CONS.
     DISPLAY NC"＃ケーヨー植物　西ＣＮＴ＝"  JCA-CNTKEIYO4
                                             UPON CONS.
*#2018/02/19 NAV ED
     DISPLAY NC"＃対象外　　　　　ＣＮＴ＝"  JCA-CNTH
                                             UPON CONS.
     DISPLAY NC"＃ＤＣＭ受領データＣＮＴ＝"  JYR-CNT
                                             UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
