# SNJ8010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ8010B.COB`

## ソースコード

```cobol
****************************************************************
*    2010/09/03 新受配信システムへ切り替え                     *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受配信管理システム　　　　　　　　*
*    モジュール名　　　　：　在庫引当／売上更新                *
*    作成日／更新日　　　：　2010/09/03                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　１．取引先Ｍ存在チェック          *
*                            ２．店舗Ｍ存在チェック            *
*                            ３．商品名称Ｍ存在チェック        *
*                            ４．商品変換テーブル存在チェック  *
*                            ５．エラー時、エラーリスト出力    *
*                            ６．商品在庫Ｍ引当済数量更新      *
*                            ７．売上伝票Ｆ更新                *
*                            ８．当日スケジュールマスタ更新    *
*                            ９．実行制御Ｆ更新                *
*    2001/02/26 NAV 不具合修正（ＰＧ終了方法）                 *
*    2004/10/14 NAV トステムビバ対応                           *
*    2005/09/02 NAV ダイユーエイト対応                         *
*    2009/09/15     仕入単価についての変更                     *
*    2010/09/03     新受配信システムへ切り替え                 *
*    2010/11/29     ダイユーエイト対応を停止                   *
*    2010/11/29     九州分　ナフコ追加                         *
*    2011/10/11 YOSHIDA.M　基幹サーバ統合                      *
*    2014/01/17 NAV TAKAHASHI　西尾閉鎖　フラワーロジ対応      *
*    2018/10/19 NAV TAKAHASHI　単価チェック変更（バグ修正）    *
*    2020/03/02 NAV TAKAHASHI　ドイトコーナン合併対応　　　　  *
*    2020/04/28 NAV TAKAHASHI　Ｄ３６５連携対応　　　　　　　  *
*    2020/07/09 NAV TAKAHASHI　Ｄ３６５連携対応（伝番採番変更）*
*    2020/08/02 NAV TAKAHASHI　Ｄ３６５連携対応（項目初期化）　*
*    2022/09/14 NAV INOUE    　変換エラーファイル出力追加　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNJ8010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/09/03.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT
     YA          IS     PITCH-2
     YB          IS     PITCH-15
     YB-21       IS     BAIKAKU.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  オンライン変換後売上伝票データ  >>---*
     SELECT   JHSHENF   ASSIGN    TO        DA-01-VI-JHSHENL3
                        ORGANIZATION        IS    INDEXED
                        ACCESS    MODE      IS    SEQUENTIAL
                        RECORD    KEY       IS    HEN-F46
                                                  HEN-F47  HEN-F01
                                                  HEN-F07  HEN-F02
                                                  HEN-F03
                        FILE      STATUS    IS    HEN-ST.
*---<<  特販部売上伝票ファイル  >>---*バッチ_チェック
     SELECT   SHTDENF   ASSIGN              DA-01-VI-SHTDENLA
                        ORGANIZATION        IS    INDEXED
                        ACCESS    MODE      IS    RANDOM
                        RECORD    KEY       IS    DEN-F46  DEN-F47
                                                  DEN-F01  DEN-F48
                                                  DEN-F02  DEN-F04
***2011.10.11 ST
***                                               DEN-F051 DEN-F03
                                                  DEN-F051 DEN-F07
                                                  DEN-F112 DEN-F03
***2011.10.11 EN
                        FILE      STATUS    IS    DEN-ST.
*---<<  特販部売上伝票ファイル  >>---*伝票_キーチェック
     SELECT   SHTDENL1  ASSIGN              DA-01-VI-SHTDENL1
                        ORGANIZATION        IS    INDEXED
                        ACCESS    MODE      IS    RANDOM
                        RECORD    KEY       IS    DE1-F01 DE1-F02
                                                  DE1-F04 DE1-F051
***2011.10.11 ST
                                                  DE1-F07 DE1-F112
***2011.10.11 EN
                                                  DE1-F03
                        FILE      STATUS    IS    DE1-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-ST.
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
*---<<  当日スケジュールマスタ  >>---*
     SELECT  JSMDAYF   ASSIGN     TO        DA-01-VI-JSMDAYL1
                       ORGANIZATION         IS   INDEXED
                       ACCESS     MODE      IS   RANDOM
                       RECORD     KEY       IS   TJS-F01
                                                 TJS-F02
                                                 TJS-F03
                       FILE       STATUS    IS   TJS-ST.
*---<<  重複伝票番号管理マスタ  >>---*
     SELECT  DENKANF   ASSIGN     TO        DA-01-VI-DENKANL1
                       ORGANIZATION         IS   INDEXED
                       ACCESS     MODE      IS   RANDOM
                       RECORD     KEY       IS   KAN-F01
                                                 KAN-F02
                                                 KAN-F03
                                                 KAN-F04
                       FILE       STATUS    IS   KAN-ST.
*---<<  プリンタ  >>---*
     SELECT   PRTF     ASSIGN               LP-04-PRTF
                       FILE      STATUS     IS   PRT-ST.
*---<<  売上二重計上Ｆ  >>---*
     SELECT   SHTDENWK ASSIGN               DA-01-S-SHTDENWK
                       FILE      STATUS     IS   DWK-ST.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
*---<<  ＳＵＢ商品変換ＴＢＬ  >>---*
     SELECT   SUBTBLF   ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F01
                                                 SUB-F02
                        FILE      STATUS    IS   SUB-ST.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*2022/09/14↓
*---<<  オンライン伝票変換エラーファイル  >>---*
     SELECT   ONLERRF   ASSIGN    TO        DA-01-VI-ONLERRL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   ERR-F01
                                                 ERR-F02
                                                 ERR-F03
                                                 ERR-F04
                                                 ERR-F05
                                                 ERR-F06
                                                 ERR-F07
                                                 ERR-F08
                                                 ERR-F09
                        FILE      STATUS    IS   ERR-ST.
*2022/09/14↑
*
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  変換後売上伝票ファイル  >>---*
 FD  JHSHENF.
     COPY        JHSHENF   OF        XFDLIB
                 JOINING   HEN       PREFIX.
*---<<  ＯＵＴファイル  >>---*
 FD  SHTDENF.
     COPY        SHTDENF   OF        XFDLIB
                 JOINING   DEN       PREFIX.
*---<<  ＯＵＴファイル  >>---*
 FD  SHTDENL1.
     COPY        SHTDENF   OF        XFDLIB
                 JOINING   DE1       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  当日スケジュールマスタ  >>---*
 FD  JSMDAYF.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
*---<<  重複伝票番号管理マスタ  >>---*
 FD  DENKANF.
     COPY     DENKANF   OF        XFDLIB
              JOINING   KAN       PREFIX.
*---<<  プリンタ >>---*
 FD  PRTF
     LABEL    RECORD     IS        OMITTED
     LINAGE              IS        66.
 01  PRT-REC            PIC  X(200).
*---<<  売上二重Ｆ >>---*
 FD  SHTDENWK.
     COPY        SHTDENF   OF        XFDLIB
                 JOINING   DWK       PREFIX.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
*---<<  ＳＵＢ商品変換ＴＢＬ  >>---*
 FD  SUBTBLF.
     COPY     SUBTBLF   OF        XFDLIB
              JOINING   SUB       PREFIX.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*2022/09/14↓
*---<<  オンライン伝票変換エラーファイル  >>---*
 FD  ONLERRF.
     COPY     ONLERRF   OF        XFDLIB
              JOINING   ERR       PREFIX.
*2022/09/14↑
*
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  HEN-ST              PIC  X(02).
     03  DEN-ST              PIC  X(02).
     03  DE1-ST              PIC  X(02).
     03  ZAI-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  SHO-ST              PIC  X(02).
     03  MEI-ST              PIC  X(02).
     03  TJS-ST              PIC  X(02).
     03  KAN-ST              PIC  X(02).
     03  PRT-ST              PIC  X(02).
     03  DWK-ST              PIC  X(02).
*# 2020/04/28 NAV ST Ｄ３６５連携対応
     03  SUB-ST              PIC  X(02).
     03  JYO-ST              PIC  X(02).
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*2022/09/14↓
     03  ERR-ST              PIC  X(02).
*2022/09/14↑
*頁カウント
 01  WK-PAGE                 PIC  9(03)  VALUE ZERO.
*フラグワーク
 01  FLG-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE SPACE.
     03  ERR-FLG             PIC  X(03)  VALUE SPACE.
     03  DBR-FLG             PIC  X(03)  VALUE SPACE.
     03  DBR-FLG1            PIC  X(03)  VALUE SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  HTENMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  SHTDENF-INV1-FLG    PIC  X(03)  VALUE SPACE.
     03  SHTDENF-INV2-FLG    PIC  X(03)  VALUE SPACE.
     03  JSMDAYF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JHMJIKF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  DENKANF-INV-FLG     PIC  X(03)  VALUE SPACE.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
     03  SUBTBLF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  WK-SENKOU-KBN       PIC  X(01)  VALUE SPACE.
     03  HJYOKEN-INV-FLG     PIC  X(03)  VALUE SPACE.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*2022/09/14↓
     03  ER01                PIC  9(01)  VALUE ZERO.
     03  ER02                PIC  9(01)  VALUE ZERO.
     03  ER03                PIC  9(01)  VALUE ZERO.
     03  ER04                PIC  9(01)  VALUE ZERO.
     03  ER05                PIC  9(01)  VALUE ZERO.
     03  ER06                PIC  9(01)  VALUE ZERO.
     03  ER07                PIC  9(01)  VALUE ZERO.
     03  ER08                PIC  9(01)  VALUE ZERO.
     03  ER09                PIC  9(01)  VALUE ZERO.
     03  ER10                PIC  9(01)  VALUE ZERO.
*2022/09/14↑
*
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*伝票番号退避
 01  WK-HEN-F02              PIC  9(09)  VALUE ZERO.
 01  WK-HEN-F07              PIC  9(05)  VALUE ZERO.
 01  WK-HEN-DENNO            PIC  9(09)  VALUE ZERO.
 01  WK-DENPYO               PIC  9(09)  VALUE ZERO.
*伝票番号管理
 01  WK-DENNO.
     03  WK-DENNO-1          PIC  9(02)  VALUE ZERO.
     03  WK-DENNO-2          PIC  9(07)  VALUE ZERO.
 01  WK-DENNO1.
     03  WK-DENNO1-1         PIC  9(01)  VALUE ZERO.
     03  WK-DENNO1-2         PIC  9(08)  VALUE ZERO.
*---<<  日付・時間ワーク  追加  96/07/29  >>---*
 01  SYS-DATE                PIC  9(06)  VALUE ZERO.
 01  HEN-DATE                PIC  9(08)  VALUE ZERO.
 01  SYS-TIME                PIC  9(08)  VALUE ZERO.
*---<<  売価チェック区分  >>---*
 01  WK-TOK-F96              PIC  X(01)  VALUE SPACE.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
*---<<  伝票ブレイクキー格納  >>---*
 01  NEW-BRK-KEY.
     03  NEW-KEY-F46         PIC  9(08)  VALUE ZERO.
     03  NEW-KEY-F47         PIC  9(04)  VALUE ZERO.
     03  NEW-KEY-F01         PIC  9(08)  VALUE ZERO.
     03  NEW-KEY-F07         PIC  9(05)  VALUE ZERO.
     03  NEW-KEY-F02         PIC  9(09)  VALUE ZERO.
 01  OLD-BRK-KEY.
     03  OLD-KEY-F46         PIC  9(08)  VALUE ZERO.
     03  OLD-KEY-F47         PIC  9(04)  VALUE ZERO.
     03  OLD-KEY-F01         PIC  9(08)  VALUE ZERO.
     03  OLD-KEY-F07         PIC  9(05)  VALUE ZERO.
     03  OLD-KEY-F02         PIC  9(09)  VALUE ZERO.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HEN-ERR           PIC N(15) VALUE
                        NC"変換売上伝票ＤＴエラー".
     03  DEN-ERR           PIC N(15) VALUE
                        NC"売上伝票ＤＴエラー".
     03  DE1-ERR           PIC N(15) VALUE
                        NC"売上伝票ＤＴエラー（ＬＦ１）".
     03  ZAI-ERR           PIC N(15) VALUE
                        NC"商品在庫Ｍエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先Ｍエラー".
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗Ｍエラー".
     03  SHO-ERR           PIC N(15) VALUE
                        NC"商品変換Ｔエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称Ｍエラー".
     03  TJS-ERR           PIC N(15) VALUE
                        NC"当日スケジュールＭエラー".
     03  KAN-ERR           PIC N(15) VALUE
                        NC"重複伝票番号管理Ｍエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンタＦエラー".
     03  DWK-ERR           PIC N(15) VALUE
                        NC"売上二重Ｆエラー".
*# 2020/04/28 NAV ST Ｄ３６５連携対応
     03  SUB-ERR           PIC N(15) VALUE
                        NC"ＳＵＢ商品変換ＴＢＬエラー".
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*2022/09/14↓
     03  ERR-ERR           PIC N(15) VALUE
                        NC"伝票変換エラーファイル異常".
*2022/09/14↑
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*見出し*
 01  LIST-M1.
     03  FILLER  CHARACTER TYPE      BAIKAKU.
         05  FILLER        PIC X(30) VALUE     SPACE.
         05  FILLER        PIC N(21) VALUE
           NC"【　オンラインデータ　変換エラーリスト　】".
         05  FILLER        PIC X(07) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"処理日".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LSYS-YY       PIC 9(02).
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-MM       PIC Z9.
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-DD       PIC Z9.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(01) VALUE
           NC"頁".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LPAGE         PIC ZZ9.
 01  LIST-M2.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"伝票_".
         05  FILLER        PIC X(04) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"取引先".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC N(02) VALUE
           NC"店舗".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"店舗名".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"取引先商品".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(05) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"商品名".
         05  FILLER        PIC X(17) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ売単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ売単価".
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"エラー内容".
*明細行*
 01  LIST-D.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  L-DEN         PIC 9(09).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TOR         PIC 9(08).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENCD       PIC 9(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENNM       PIC X(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHOCD       PIC X(13).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHONM       PIC X(18).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-ERR         PIC N(10).
 01  DUMMY                 PIC X(01) VALUE     SPACE.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*#2020/04/28 NAV ST Ｄ３６５伝票番号取得
 01  WK-D365-DEN-PARA.
     03  WK-D365-PARA-IN1   PIC   X(01).
     03  WK-D365-PARA-IN2   PIC   9(08).
     03  WK-D365-PARA-OUT1  PIC   X(20).
     03  WK-D365-PARA-OUT2  PIC   X(01).
*****Ｄ３６５伝票番号採番値エリア
     COPY   HJYOKEN  OF XFDLIB  JOINING   JD3  AS   PREFIX.
*伝票番号編集
 01  WK-RENBAN               PIC  9(08).
 01  WK-D365-DEN.
     03  WK-D365-TYPE        PIC  X(01).
     03  WK-D365-TOKCD       PIC  9(08).
     03  WK-D365-DATE        PIC  9(06).
     03  WK-D365-RENBAN      PIC  9(05).
*#2020/07/09 NAV ST Ｄ３６５伝票番号採番方法変更
 01  WK-D3652-DEN.
     03  WK-D3652-TYPE        PIC  X(01).
     03  WK-D3652-DATE        PIC  9(08).
     03  WK-D3652-RENBAN      PIC  9(06).
     03  WK-D3652-ORDER       PIC  X(05).
*#2020/07/09 NAV ED Ｄ３６５伝票番号採番方法変更
*システム日付編集
 01  WK-D365-SYSDT           PIC  9(09)V9(02).
 01  FILLER                  REDEFINES  WK-D365-SYSDT.
     03  WK-D365-SYSDT1      PIC  9(01).
     03  WK-D365-SYSDT2      PIC  9(08).
     03  WK-D365-SYSDT3      PIC  9(02).
*#2020/04/28 NAV ED Ｄ３６５伝票番号取得
****************************************************************
 LINKAGE               SECTION.
 01  LINK-HIDUKE           PIC 9(08).
 01  LINK-JIKAN            PIC 9(04).
 01  LINK-TORICD           PIC 9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION  USING  LINK-HIDUKE
                                         LINK-JIKAN
                                         LINK-TORICD.
**
 DECLARATIVES.
 HEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHSHENF.
     MOVE        HEN-ST      TO        E-ST.
     MOVE        "JHSHENF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     HEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     MOVE        DEN-ST      TO        E-ST.
     MOVE        "SHTDENLA"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     DEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     DISPLAY "DEN-F46  = " DEN-F46  UPON CONS.
     DISPLAY "DEN-F47  = " DEN-F47  UPON CONS.
     DISPLAY "DEN-F01  = " DEN-F01  UPON CONS.
     DISPLAY "DEN-F48  = " DEN-F48  UPON CONS.
     DISPLAY "DEN-F02  = " DEN-F02  UPON CONS.
     DISPLAY "DEN-F04  = " DEN-F04  UPON CONS.
     DISPLAY "DEN-F051 = " DEN-F051 UPON CONS.
     DISPLAY "DEN-F07  = " DEN-F07  UPON CONS.
     DISPLAY "DEN-F112 = " DEN-F112 UPON CONS.
     DISPLAY "DEN-F03  = " DEN-F03  UPON CONS.
     STOP        RUN.
 DE1-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENL1.
     MOVE        DE1-ST      TO        E-ST.
     MOVE        "SHTDENL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     DE1-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     MOVE        ZAI-ST      TO        E-ST.
     MOVE        "ZAMZAIF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     ZAI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST      TO        E-ST.
     MOVE        "HTOKMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TOK-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     MOVE        TEN-ST      TO        E-ST.
     MOVE        "HTENMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     MOVE        SHO-ST      TO        E-ST.
     MOVE        "HSHOTBL"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     SHO-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST      TO        E-ST.
     MOVE        "HMEIMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     MEI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TJS-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JSMDAYF.
     MOVE        TJS-ST      TO        E-ST.
     MOVE        "JSMDAYF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TJS-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 KAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE DENKANF.
     MOVE        KAN-ST      TO        E-ST.
     MOVE        "DENKANF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     KAN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PRTF.
     MOVE        PRT-ST      TO        E-ST.
     MOVE        "PRTF   "   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     PRT-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 DWK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENWK.
     MOVE        DWK-ST      TO        E-ST.
     MOVE        "SHTDENWK"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     DWK-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 SUB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SUBTBLF.
     MOVE        SUB-ST      TO        E-ST.
     MOVE        "SUBTBLL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     SUB-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
*2022/09/14↓
 ERR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ONLERRF.
     MOVE        ERR-ST      TO        E-ST.
     MOVE        "ONLERRL1"  TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     ERR-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
*2022/09/14↑
 END     DECLARATIVES.
****************************************************************
 PROCESS-START               SECTION.
*
     DISPLAY  "PROCESS-START"  UPON CONS.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC  UNTIL   END-FLG  =  "END".
     PERFORM       END-SEC.
     STOP      RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*****OPEN     I-O       ZAMZAIF  SHTDENF  JHSHENF  SHTDENL1.
*****OPEN     INPUT     HTOKMS   HTENMS   HSHOTBL  HMEIMS.
*****OPEN     EXTEND    SHTDENWK.
     OPEN     I-O       ZAMZAIF  SHTDENF  JHSHENF
                        DENKANF.
*2022/09/14↓
     OPEN     I-O       ONLERRF.
*2022/09/14↑
     OPEN     INPUT     HTOKMS   HTENMS   HSHOTBL  HMEIMS
*# 2020/04/28 NAV ST Ｄ３６５連携対応
************************SHTDENL1.
                        SHTDENL1  SUBTBLF.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
     OPEN     EXTEND    SHTDENWK.
*エラーリスト印字エリア明細クリア
     MOVE     SPACE               TO   LIST-D.
*システム日付／時刻取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   HEN-DATE.
     ACCEPT    SYS-TIME         FROM   TIME.
*# 2020/04/28 NAV ST Ｄ３６５連携対応************************
     OPEN   INPUT  HJYOKEN.
     PERFORM  HJYOKEN-READ-SEC.
*****DISPLAY "HJYOKEN-INV-FLG1 = " HJYOKEN-INV-FLG UPON CONS.
     IF  HJYOKEN-INV-FLG = SPACE
         MOVE  JYO-REC          TO      JD3-REC
     ELSE
         MOVE  SPACE            TO      JD3-REC
         INITIALIZE                     JD3-REC
         MOVE  60               TO      JD3-F01
         MOVE  "D365NOE"        TO      JD3-F02
         MOVE  NC"Ｄ３６５伝票番号（ＯＮＬ分）"
                                TO      JD3-F03
         MOVE  1                TO      JD3-F04
         MOVE  1                TO      JD3-F05
*#2020/07/09 NAV ST
*********MOVE  99999            TO      JD3-F06
         MOVE  999999           TO      JD3-F06
*#2020/07/09 NAV ED
         MOVE  HEN-DATE         TO      JD3-F07
     END-IF.
     CLOSE        HJYOKEN.
*# 2020/04/28 NAV ST Ｄ３６５連携対応************************
     DISPLAY "## D365" NC"伝票" "NO-" NC"開始" "NO = " JD3-F04
             " ##" UPON CONS.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*END-FLG CLEAR
     MOVE      SPACE         TO     END-FLG.
*変換後売上伝票ファイルスタート
     MOVE      SPACE         TO     HEN-REC.
     INITIALIZE                     HEN-REC.
     MOVE      LINK-HIDUKE   TO     HEN-F46.
     MOVE      LINK-JIKAN    TO     HEN-F47.
     MOVE      LINK-TORICD   TO     HEN-F01.
     MOVE      ZERO          TO     HEN-F07.
     MOVE      ZERO          TO     HEN-F02.
     MOVE      ZERO          TO     HEN-F03.
     START     JHSHENF  KEY  IS  >=   HEN-F46 HEN-F47 HEN-F01
                                      HEN-F07 HEN-F02 HEN-F03
      INVALID
         MOVE     "END"      TO     END-FLG
         DISPLAY NC"＃伝票更新エラー（ＳＴ）＃" UPON CONS
         DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
         DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
         DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
         GO                  TO     MAIN010
      NOT  INVALID
         PERFORM   JHSHENF-READ-SEC
         IF   END-FLG = "END"
              DISPLAY NC"＃伝票更新エラー（ＳＴ）＃" UPON CONS
              DISPLAY NC"＃受信日付" " = " LINK-HIDUKE UPON CONS
              DISPLAY NC"＃受信時間" " = " LINK-JIKAN  UPON CONS
              DISPLAY NC"＃取引先　" " = " LINK-TORICD UPON CONS
              GO                  TO     MAIN010
         END-IF
     END-START.
*
     OPEN     OUTPUT    PRTF.
     PERFORM   SHTDENF-CRT-SEC  UNTIL  END-FLG =  "END".
     CLOSE              PRTF.
*当日スケジュールマスタ更新
     PERFORM JSMDAYF-REWRITE-SEC.
 MAIN010.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      2.1.1　売上伝票ファイル作成                            *
****************************************************************
 SHTDENF-CRT-SEC        SECTION.
*
     MOVE     "SHTDENF-CRT-SEC"   TO   S-NAME.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
      MOVE    HEN-F46            TO   NEW-KEY-F46.
      MOVE    HEN-F47            TO   NEW-KEY-F47.
      MOVE    HEN-F01            TO   NEW-KEY-F01.
      MOVE    HEN-F07            TO   NEW-KEY-F07.
      MOVE    HEN-F02            TO   NEW-KEY-F02.
     IF   NEW-KEY-F46  =  OLD-KEY-F46
     AND  NEW-KEY-F47  =  OLD-KEY-F47
     AND  NEW-KEY-F01  =  OLD-KEY-F01
     AND  NEW-KEY-F07  =  OLD-KEY-F07
     AND  NEW-KEY-F02  =  OLD-KEY-F02
          CONTINUE
     ELSE
**********Ｄ３６５伝票番号採番
          PERFORM  SAIBAN
          MOVE  NEW-KEY-F46      TO   OLD-KEY-F46
          MOVE  NEW-KEY-F47      TO   OLD-KEY-F47
          MOVE  NEW-KEY-F01      TO   OLD-KEY-F01
          MOVE  NEW-KEY-F07      TO   OLD-KEY-F07
          MOVE  NEW-KEY-F02      TO   OLD-KEY-F02
     END-IF.
*フラグ初期化
     INITIALIZE         KEP-FLG  ERR-FLG  CHK-FLG.
*変換後売上伝票Ｆエラーチェック
     PERFORM   ERR-CHK-SEC.
*二重計上チェック（二重計上の場合は更新済みにする）
     IF  CHK-FLG  =  "CHK"
         MOVE         1       TO     HEN-F54
         MOVE         1       TO     HEN-F55
*        エラーフラグ更新へ
         REWRITE      HEN-REC
         GO                   TO     CHK010
     END-IF.
*在庫引当処理
     IF  HSHOTBL-INV-FLG  =  SPACE
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     HEN-F27D
         END-IF
         MOVE         1       TO     HEN-F262
     END-IF.
*エラー時、エラー区分セット
     IF  ERR-FLG  =  "ERR"
         MOVE         1       TO     HEN-F55
     END-IF.
*更新済みＦＬＧセット
     MOVE          1          TO     HEN-F54.
*変換後レコードセット
     MOVE          SPACE      TO     DEN-REC.
     INITIALIZE                      DEN-REC.
     MOVE          HEN-REC    TO     DEN-REC.
*エラーフラグ更新へ
     REWRITE       HEN-REC.
*ダブリ伝票番号の時
*#   IF   DBR-FLG1 = "CHK"
*#        MOVE  WK-HEN-DENNO  TO     DEN-F02
*#        MOVE  WK-HEN-DENNO  TO     DEN-F23
*#   END-IF.
*内部統制項目更新
     MOVE        "99"          TO     DEN-F59  DEN-F60  DEN-F61.
     MOVE        HEN-DATE      TO     DEN-F62  DEN-F63  DEN-F64.
     MOVE        "1"           TO     DEN-F65  DEN-F66.
     MOVE        SYS-TIME(1:6) TO     DEN-F67.
     MOVE        ZERO          TO     DEN-F413.
*小売連携区分取得 2011/10/31 ST *********************
*TTTTIF  HMEIMS-INV-FLG  = SPACE
*********DISPLAY "MEI-F10 = " MEI-F10 UPON CONS
*TTTTTTTTMOVE    MEI-F10       TO     DEN-F32
*TTTTEND-IF.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
     IF  HMEIMS-INV-FLG  = SPACE
     AND HSHOTBL-INV-FLG = SPACE
     AND SUBTBLF-INV-FLG = SPACE
*TTTT    IF  DEN-F32  =  SPACE
************ MOVE WK-SENKOU-KBN       TO    DEN-F32
*TTTT        IF   WK-SENKOU-KBN = "1"
*TTTT             MOVE  "2"           TO    DEN-F32
*TTTT        ELSE
*TTTT             IF  WK-SENKOU-KBN  =  "2"
*TTTT                 MOVE  "3"       TO    DEN-F32
*TTTT             ELSE
*TTTT                 MOVE  SPACE     TO    DEN-F32
*TTTT             END-IF
*TTTT       END-IF
*TTTT    END-IF
*2020/12/14 NAV ST ＳＵＢ商品変換ＴＢＬオーダー区分セット
         MOVE     SUB-F19             TO    DEN-F32
*2020/12/14 NAV ED ＳＵＢ商品変換ＴＢＬオーダー区分セット
     END-IF.
*# 2021/08/02 NAV ST Ｄ３６５連携項目初期化
     MOVE    ZERO                     TO    DEN-D86  DEN-D87.
     MOVE    SPACE                    TO    DEN-D85  DEN-D88
                                            DEN-D89  DEN-D90
                                            DEN-D91  DEN-D92
                                            DEN-D93  DEN-D94
                                            DEN-D95  DEN-D96
                                            DEN-D97  DEN-D98.
*# 2021/08/02 NAV ED Ｄ３６５連携項目初期化
*#2020/07/09 NAV ST
*****MOVE    WK-D365-DEN              TO    DEN-D99.
     MOVE    WK-D3652-DEN             TO    DEN-D99.
*#2020/07/09 NAV ED
*****DISPLAY "WK-D365-DEN = " WK-D365-DEN UPON CONS.
*****DISPLAY "DEN-D99     = " DEN-D99     UPON CONS.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*****************************************************
*売上伝票ファイル書き込み
 CHK000.
     WRITE         DEN-REC.
 CHK010.
*売上伝票ファイル読込み
     PERFORM       JHSHENF-READ-SEC.
*
 SHTDENF-CRT-EXIT.
     EXIT.
****************************************************************
*      2.1.1　売上伝票ファイル作成                            *
****************************************************************
 ERR-CHK-SEC            SECTION.
*
     MOVE     "ERR-CHK-SEC"       TO   S-NAME.
*
*2022/09/14↓
     MOVE      0           TO        ER01
                                     ER02
                                     ER03
                                     ER04
                                     ER05
                                     ER06
                                     ER07
                                     ER08
                                     ER09
                                     ER10.
     MOVE      SPACE       TO        ERR-REC.
     INITIALIZE                      ERR-REC.
*2022/09/14↑
*
*取引先マスタ存在チェック*
     PERFORM     HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE NC"取引先マスタ　未登録" TO      L-ERR
*2022/09/14↓
         MOVE    1         TO        ER01
*2022/09/14↑
         PERFORM           ERR-EDT-SEC
         PERFORM           ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
         MOVE    TOK-F52   TO        HEN-F24
         MOVE    TOK-F86   TO        HEN-F279  HEN-F27A
     END-IF.
*店舗マスタ存在チェック*
     PERFORM     HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE NC"店舗　マスタ　未登録" TO      L-ERR
*2022/09/14↓
         MOVE    1         TO        ER02
*2022/09/14↑
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
         MOVE    99        TO        HEN-F06
     END-IF.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
     PERFORM     SUBTBLF-READ-SEC.
     IF  SUBTBLF-INV-FLG  =  "INV"
         MOVE NC"ＳＵＢ商変ＴＬ未登録"  TO     L-ERR
*2022/09/14↓
         MOVE    1         TO        ER03
*2022/09/14↑
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     END-IF.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
     IF  HSHOTBL-INV-FLG  =  "INV"
         MOVE NC"商品変換ＴＢＬ未登録" TO      L-ERR
*2022/09/14↓
         MOVE    1         TO        ER04
*2022/09/14↑
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG

****09/09/02     仕入単価追加
*
*    MOVE      HEN-F1411   TO     MEI-F011
*    MOVE      HEN-F1412   TO     MEI-F012
*    READ      HMEIMS
*              INVALID
*                  CONTINUE
*              NOT  INVALID
*                  MOVE      MEI-F041 TO    HEN-F171
*    END-READ
     ELSE
*        *商品名称マスタチェック*
*#2020/03/02 NAV ST ３８７０９、２３６３は変換時の出荷場所にする
*********IF   HEN-F01  NOT =  38709
*             MOVE    SHO-F04   TO        HEN-F08   HEN-F09
*********END-IF
*********ＬＩＸＩＬ、コーナンの場合は、受信時の出荷場所を
*********使用する。以外の取引先は変換ＴＢＬより再セット
         IF   HEN-F01  =  38709
         OR   HEN-F01  =  2363
              CONTINUE
         ELSE
              MOVE    SHO-F04   TO        HEN-F08   HEN-F09
         END-IF
*#2020/03/02 NAV ED ３８７０９、２３６３は変換時の出荷場所にする
*********DISPLAY "TOKCD = " HEN-XI0004(40:6)  UPON CONS
*********##2014/01/17 NAV ST
         IF   HEN-F01  =  173  AND  HEN-XI0004(40:6) = "001460"
**************MOVE  "86"   TO        HEN-F08   HEN-F09
              MOVE  "TV"   TO        HEN-F08   HEN-F09
         END-IF
*********##2014/01/17 NAV ED
         IF   HEN-F01  =  173  AND  HEN-XI0004(40:6) = "001473"
              MOVE  "84"   TO        HEN-F08   HEN-F09
         END-IF
         IF   HEN-F01  =  173  AND  HEN-XI0004(40:6) = "001647"
              MOVE  "7S"   TO        HEN-F08   HEN-F09
         END-IF
         MOVE    SHO-F031  TO        HEN-F1411 MEI-F011
         MOVE    SHO-F032  TO        HEN-F1412 MEI-F012
         PERFORM  HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  "INV"
             MOVE NC"商品名称マスタ未登録" TO  L-ERR
*2022/09/14↓
             MOVE      1             TO        ER05
*2022/09/14↑
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
             MOVE      "ERR"         TO        ERR-FLG
         ELSE
             IF    HEN-F1421  =  SPACE
                   MOVE MEI-F031     TO        HEN-F1421
                   MOVE MEI-F032     TO        HEN-F1422
             END-IF
*            MOVE          MEI-F041  TO        HEN-F171
         END-IF
*        *単価チェック* 2004/04/30 売価金額のチェック無し
*        *単価チェック* 2004/07/16 取引先Ｍよりﾁｪｯｸ区分取得
*********IF  (HEN-F172 NOT = SHO-F05) OR
*********    (HEN-F173 NOT = SHO-F06)
         IF  (HEN-F172 NOT = SHO-F05)
         OR  (HEN-F173 NOT = SHO-F06  AND  WK-TOK-F96  =  "0")
*********OR ((HEN-F01      = 137607) AND (HEN-F173 NOT = SHO-F06))
             MOVE NC"単価が違います！　　" TO  L-ERR
*2022/09/14↓
             MOVE      1             TO        ER06
*2022/09/14↑
*************#2018/10/19 NAV ST
*************IF  (DEN-F172 NOT = SHO-F05)
****#########IF  (HEN-F172 NOT = SHO-F05)
*************#2018/10/19 NAV ED
                 MOVE      SHO-F05   TO        L-MGEN
****#########END-IF
*************#2018/10/19 NAV ST
*************IF  (DEN-F173 NOT = SHO-F06)
****#########IF  (HEN-F173 NOT = SHO-F06)
*************#2018/10/19 NAV ED
                 MOVE      SHO-F06   TO        L-MURI
****#########END-IF
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
         END-IF
*
***09/9/1    仕入単価追加
         IF  (SHO-F09  >     ZERO   )
             MOVE      SHO-F09   TO        HEN-F171
         ELSE
             MOVE      MEI-F041  TO        HEN-F171
         END-IF
     END-IF.
*
     MOVE     HEN-F02    TO   WK-DENPYO.
 DENERR010.
*売上伝票ファイル存在チェック（二重計上防止）
     PERFORM  SHTDENF-READ-SEC.
     PERFORM  SHTDENF-READ1-SEC.
*****DISPLAY "INV-FLG1 = " SHTDENF-INV1-FLG UPON CONS.
*****DISPLAY "INV-FLG2 = " SHTDENF-INV2-FLG UPON CONS.
*****IF  SHTDENF-INV1-FLG  =  SPACE
*****OR  SHTDENF-INV2-FLG  =  SPACE
*****    MOVE NC"売上二重計上"         TO      L-ERR
*****    PERFORM       ERR-EDT-SEC
*****    PERFORM       ERR-WRT-SEC
*****    MOVE     HEN-REC  TO        DWK-REC
*****    WRITE    DWK-REC
*****    MOVE    "CHK"     TO        CHK-FLG
*****END-IF.
*#   IF   DBR-FLG = "CHK"
*#   AND  SHTDENF-INV1-FLG = "INV"
*#   AND  SHTDENF-INV2-FLG = "INV"
*#        MOVE  "CHK"  TO  DBR-FLG1
*#        MOVE  SPACE  TO  DBR-FLG
*#   END-IF.
*#   IF   DBR-FLG1 = "CHK"
*#   AND  HEN-F02  =  WK-HEN-F02
*#   AND  HEN-F07  =  WK-HEN-F07
*#        MOVE   WK-DENNO  TO  WK-HEN-DENNO
*#        GO               TO  DENERR020
*#   ELSE
**********MOVE   SPACE     TO  DBR-FLG
*#        MOVE   SPACE     TO  DBR-FLG1
**********MOVE   ZERO      TO  WK-DENNO
*#   END-IF.
*#   IF   DBR-FLG = SPACE  AND DBR-FLG1 = SPACE
*#        MOVE   ZERO      TO  WK-DENNO
*#   END-IF.
*****ダブリ伝票番号が存在した場合
     IF  SHTDENF-INV1-FLG  =  SPACE
     OR  SHTDENF-INV2-FLG  =  SPACE
*********トステムビバの時
*********IF  DEN-F01  =  38709 OR 1225
*2010/11/29 ダイユーエイトを外し、九州ナフコ追加 NAV
*ダブリ対応処理廃止キー追加により 2011/10/31 *****
*#       IF  DEN-F01  =  38709 OR  137607
*#           IF   DBR-FLG = SPACE
*#                MOVE NC"売上二重計上（参照）" TO      L-ERR
*#                PERFORM       ERR-EDT-SEC
*#                PERFORM       ERR-WRT-SEC
*#                MOVE     HEN-REC  TO        DWK-REC
*#                WRITE    DWK-REC
*#                MOVE     HEN-F02  TO        WK-HEN-F02
*#                MOVE     HEN-F07  TO        WK-HEN-F07
*#                MOVE     HEN-F02  TO        WK-DENNO WK-DENNO1
*#           END-IF
*#           MOVE    "CHK"     TO      DBR-FLG
*#           IF  DEN-F01  =  38709
*#               ADD     1         TO      WK-DENNO-1
*#               MOVE    WK-DENNO  TO      WK-DENPYO
*#           END-IF
*2010/11/29 ダイユーエイトを外し、九州ナフコ追加 NAV
*************IF  DEN-F01  =  1225
*                ADD     1         TO      WK-DENNO1-1
*                MOVE    WK-DENNO1 TO      WK-DENPYO  WK-DENNO
*************END-IF
*#           IF  DEN-F01  =  137607
*#               ADD     1         TO      WK-DENNO-1
*#               MOVE    WK-DENNO  TO      WK-DENPYO
*#           END-IF
*#           GO                TO      DENERR010
*#       ELSE
             MOVE NC"売上二重計上"         TO      L-ERR
*2022/09/14↓
             MOVE     1        TO        ER07
*2022/09/14↑
             PERFORM       ERR-EDT-SEC
             PERFORM       ERR-WRT-SEC
             MOVE     HEN-REC  TO        DWK-REC
             WRITE    DWK-REC
             MOVE    "CHK"     TO        CHK-FLG
*#       END-IF
     END-IF.
 DENERR020.
*重複伝票番号管理マスタ更新
*#   IF  DBR-FLG1 =  "CHK"
*#   AND HEN-F03  =   1
*#       PERFORM      DABURI-SEC
*#   END-IF.
*
*2022/09/14↓
     IF  ( ER01 =  0 ) AND
         ( ER02 =  0 ) AND
         ( ER03 =  0 ) AND
         ( ER04 =  0 ) AND
         ( ER05 =  0 ) AND
         ( ER06 =  0 ) AND
         ( ER07 =  0 ) AND
         ( ER08 =  0 ) AND
         ( ER09 =  0 ) AND
         ( ER10 =  0 )
           CONTINUE
     ELSE
*バッチ日付
           MOVE    HEN-F46        TO   ERR-F01
*バッチ時刻
           MOVE    HEN-F47        TO   ERR-F02
*バッチ取引先
           MOVE    HEN-F01        TO   ERR-F03
*処理日付
           MOVE    HEN-DATE       TO   ERR-F04
*処理時刻
           MOVE    SYS-TIME(1:6)  TO   ERR-F05
*伝票番号
           MOVE    HEN-F02        TO   ERR-F06
*店舗ＣＤ
           MOVE    HEN-F07        TO   ERR-F07
*納品日
           MOVE    HEN-F112       TO   ERR-F08
*行番号
           MOVE    HEN-F03        TO   ERR-F09
*出荷場所
           MOVE    HEN-F08        TO   ERR-F10
*伝発場所
           MOVE    HEN-F09        TO   ERR-F11
*発注日
           MOVE    HEN-F111       TO   ERR-F12
*相手商品ＣＤ
           MOVE    HEN-F25        TO   ERR-F13
*サカタ商品CD
           MOVE    HEN-F1411      TO   ERR-F14
*サカタ品単CD
           MOVE    HEN-F1412      TO   ERR-F15
*商品名カナ１
           MOVE    HEN-F1421      TO   ERR-F16
*商品名カナ２
           MOVE    HEN-F1422      TO   ERR-F17
*数量
           MOVE    HEN-F15        TO   ERR-F18
*原価単価
           MOVE    HEN-F172       TO   ERR-F19
*M:原価単価
           IF      HSHOTBL-INV-FLG NOT = "INV"
                   MOVE    SHO-F05        TO   ERR-F20
           END-IF
*売価単価
           MOVE    HEN-F173       TO   ERR-F21
*M:売価単価
           IF      HSHOTBL-INV-FLG NOT = "INV"
                   MOVE    SHO-F06        TO   ERR-F22
           END-IF
*エラー区分1
           IF      ER01               =  1
                   MOVE    1              TO   ERR-F23
           END-IF
*エラー区分2
           IF      ER02               =  1
                   MOVE    1              TO   ERR-F24
           END-IF
*エラー区分3
           IF      ER03               =  1
                   MOVE    1              TO   ERR-F25
           END-IF
*エラー区分4
           IF      ER04               =  1
                   MOVE    1              TO   ERR-F26
           END-IF
*エラー区分5
           IF      ER05               =  1
                   MOVE    1              TO   ERR-F27
           END-IF
*エラー区分6
           IF      ER06               =  1
                   MOVE    1              TO   ERR-F28
           END-IF
*エラー区分7
           IF      ER07               =  1
                   MOVE    1              TO   ERR-F29
           END-IF
*エラー区分8
           IF      ER08               =  1
                   MOVE    1              TO   ERR-F30
           END-IF
*エラー区分9
           IF      ER09               =  1
                   MOVE    1              TO   ERR-F31
           END-IF
*エラー区分10
           IF      ER10              =  1
                   MOVE    1              TO   ERR-F32
           END-IF
*
           WRITE   ERR-REC
     END-IF.
*2022/09/14↑
*
 ERR-CHK-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 DABURI-SEC             SECTION.
*
     MOVE     "DABURI-SEC"        TO   S-NAME.
*
     MOVE      LINK-HIDUKE        TO   KAN-F01.
     MOVE      LINK-JIKAN         TO   KAN-F02.
     MOVE      DEN-F01            TO   KAN-F03.
     MOVE      WK-HEN-DENNO       TO   KAN-F04.
     READ      DENKANF
               INVALID
               MOVE     "INV"     TO   DENKANF-INV-FLG
               NOT  INVALID
               MOVE     SPACE     TO   DENKANF-INV-FLG
     END-READ.
*
     IF        DENKANF-INV-FLG = "INV"
               MOVE     SPACE     TO   KAN-REC
               INITIALIZE              KAN-REC
               MOVE  LINK-HIDUKE  TO   KAN-F01
               MOVE  LINK-JIKAN   TO   KAN-F02
               MOVE  DEN-F01      TO   KAN-F03
               MOVE  WK-HEN-DENNO TO   KAN-F04
               MOVE  WK-HEN-F02   TO   KAN-F05
               WRITE KAN-REC
     ELSE
               DISPLAY NC"＃重複登録エラー＃" UPON CONS
               DISPLAY NC"＃伝票番号　＝　" WK-DENNO-2 UPON CONS
     END-IF.
*
 DABURI-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    HEN-F08         TO   ZAI-F01.
     MOVE    HEN-F1411       TO   ZAI-F021.
     MOVE    HEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
      MOVE      "1"           TO   KEP-FLG.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      HEN-F08       TO   ZAI-F01.
      MOVE      HEN-F1411     TO   ZAI-F021.
      MOVE      HEN-F1412     TO   ZAI-F022.
      MOVE      SHO-F08       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  HEN-F15.
*ケーヨーの時のみ発注数量３に数量加算
      IF        LINK-TORICD = 173
                COMPUTE   ZAI-F21       =    ZAI-F21  +  HEN-F15
                MOVE      "1"           TO   ZAI-F97
      END-IF.
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  MEI-F031      TO   ZAI-F30
          MOVE  HEN-DATE      TO   ZAI-F98
          MOVE  HEN-DATE      TO   ZAI-F99
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  HEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =    ZAI-F27  +  HEN-F15
*        ケーヨーの時のみ発注数量３に数量加算
         IF  LINK-TORICD = 173
             COMPUTE  ZAI-F21   =    ZAI-F21  +  HEN-F15
             MOVE    "1"        TO   ZAI-F97
         END-IF
         MOVE     HEN-DATE  TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =    ZAI-F28  +  HEN-F15
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =    ZAI-F27  +  HEN-F15
*        ケーヨーの時のみ発注数量３に数量加算
         IF  LINK-TORICD = 173
             COMPUTE  ZAI-F21   =    ZAI-F21  +  HEN-F15
             MOVE    "1"        TO   ZAI-F97
         END-IF
         MOVE     HEN-DATE  TO   ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*# 2020/04/28 NAV ST Ｄ３６５連携対応************************
     OPEN   I-O    HJYOKEN.
     PERFORM  HJYOKEN-READ-SEC.
*****DISPLAY "HJYOKEN-INV-FLG2 = " HJYOKEN-INV-FLG UPON CONS.
     IF  HJYOKEN-INV-FLG = SPACE
         MOVE  JD3-REC          TO      JYO-REC
         REWRITE JYO-REC
     ELSE
         MOVE  SPACE            TO      JYO-REC
         INITIALIZE                     JYO-REC
         MOVE  JD3-REC          TO      JYO-REC
         WRITE JYO-REC
     END-IF.
     CLOSE        HJYOKEN.
*# 2020/04/28 NAV ED Ｄ３６５連携対応************************
*各ファイルのＣＬＯＳＥ
     CLOSE    JHSHENF SHTDENF ZAMZAIF HTOKMS HTENMS HSHOTBL HMEIMS
              SHTDENL1 SHTDENWK DENKANF.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
     CLOSE    SUBTBLF.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*2022/09/14↓
     CLOSE    ONLERRF.
*2022/09/14↑
     DISPLAY NC"＃＃売上更新処理終了＃＃" UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*                  エラーリスト明細編集　　　                  *
****************************************************************
 ERR-EDT-SEC               SECTION.
*
     MOVE   "ERR-EDT-SEC"  TO        S-NAME.
     MOVE        HEN-F02   TO        L-DEN.
     MOVE        HEN-F01   TO        L-TOR.
     MOVE        HEN-F07   TO        L-TENCD.
     MOVE        HEN-F30   TO        L-TENNM.
     MOVE        HEN-F25   TO        L-SHOCD.
     MOVE        HEN-F1421 TO        L-SHONM.
     MOVE        HEN-F1422(1:3) TO   L-SHONM(16:3).
     MOVE        HEN-F172  TO        L-DGEN.
     MOVE        HEN-F173  TO        L-DURI.
***  DISPLAY "L-DEN      = " L-DEN        UPON CONS.
***  DISPLAY "L-TOR      = " L-TOR        UPON CONS.
***  DISPLAY "L-TENCD    = " L-TENCD      UPON CONS.
***  DISPLAY "L-TENNM    = " L-TENNM      UPON CONS.
***  DISPLAY "L-SHOCD    = " L-SHOCD      UPON CONS.
***  DISPLAY "L-SHONM    = " L-SHONM      UPON CONS.
***  DISPLAY "L-DGEN     = " L-DGEN       UPON CONS.
***  DISPLAY "L-DURI     = " L-DURI       UPON CONS.
***  DISPLAY "HEN-F02    = " HEN-F02      UPON CONS.
***  DISPLAY "HEN-F01    = " HEN-F01      UPON CONS.
***  DISPLAY "HEN-F07    = " HEN-F07      UPON CONS.
***  DISPLAY "HEN-F30    = " HEN-F30      UPON CONS.
***  DISPLAY "HEN-F25    = " HEN-F25      UPON CONS.
***  DISPLAY "HEN-F1421  = " HEN-F1421    UPON CONS.
***  DISPLAY "HEN-F1422  = " HEN-F1422    UPON CONS.
***  DISPLAY "HEN-F172   = " HEN-F172     UPON CONS.
***  DISPLAY "HEN-F173   = " HEN-F173     UPON CONS.
 ERR-EDT-EXIT.
     EXIT.
****************************************************************
*                  エラーリスト出力　　　　　                  *
****************************************************************
 ERR-WRT-SEC               SECTION.
*
     MOVE   "ERR-WRT-SEC"  TO        S-NAME.
     IF  (WK-PAGE = ZERO) OR (LINAGE-COUNTER > 61)
         IF  (LINAGE-COUNTER > 61)
             MOVE          SPACE     TO        PRT-REC
             WRITE         PRT-REC   AFTER     PAGE
         END-IF
         ADD     1         TO        WK-PAGE
         MOVE    WK-PAGE   TO        LPAGE
         MOVE    HEN-DATE(1:4)  TO        LSYS-YY
         MOVE    HEN-DATE(5:2)  TO        LSYS-MM
         MOVE    HEN-DATE(7:2)  TO        LSYS-DD
         WRITE   PRT-REC   FROM      LIST-M1   AFTER     1
         WRITE   PRT-REC   FROM      LIST-M2   AFTER     2
         WRITE   PRT-REC   FROM      DUMMY     AFTER     1
     END-IF.
     WRITE       PRT-REC   FROM      LIST-D    AFTER     1.
     MOVE        SPACE     TO        LIST-D.
 ERR-WRT-EXIT.
     EXIT.
****************************************************************
*                得意先マスタ読込み                            *
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE      "HTOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE      HEN-F01     TO     TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE      "INV"    TO    HTOKMS-INV-FLG
               MOVE      "0"      TO    WK-TOK-F96
               NOT  INVALID
               MOVE      SPACE    TO    HTOKMS-INV-FLG
               MOVE      TOK-F96  TO    WK-TOK-F96
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込み                              *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      HEN-F01     TO     TEN-F52.
     MOVE      HEN-F07     TO     TEN-F011.
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      HEN-F01     TO     SHO-F01.
     MOVE      HEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*                売上伝票ファイル読込み                        *
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     MOVE      "SHTDENF-READ-SEC" TO    S-NAME.
*
     MOVE      HEN-F46     TO     DEN-F46.
     MOVE      HEN-F47     TO     DEN-F47.
     MOVE      HEN-F01     TO     DEN-F01.
     MOVE      HEN-F48     TO     DEN-F48.
     MOVE      HEN-F02     TO     DEN-F02.
*####MOVE      WK-DENPYO   TO     DEN-F02.
     MOVE      HEN-F04     TO     DEN-F04.
     MOVE      HEN-F051    TO     DEN-F051.
     MOVE      HEN-F03     TO     DEN-F03.
***2011.10.11 ST
     MOVE      HEN-F07     TO     DEN-F07.
     MOVE      HEN-F112    TO     DEN-F112.
***2011.10.11 EN
     READ      SHTDENF
               INVALID
               MOVE      "INV"    TO    SHTDENF-INV1-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SHTDENF-INV1-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*                売上伝票ファイル読込み                        *
****************************************************************
 SHTDENF-READ1-SEC         SECTION.
*
     MOVE      "SHTDENF-READ1-SEC" TO    S-NAME.
*
*****MOVE      ZERO        TO     DEN-F46.
*****MOVE      ZERO        TO     DEN-F47.
     MOVE      HEN-F01     TO     DE1-F01.
*****MOVE      ZERO        TO     DEN-F48.
     MOVE      HEN-F02     TO     DE1-F02.
*####MOVE      WK-DENPYO   TO     DE1-F02.
     MOVE      HEN-F04     TO     DE1-F04.
     MOVE      HEN-F051    TO     DE1-F051.
     MOVE      HEN-F03     TO     DE1-F03.
***2011.10.11 ST
     MOVE      HEN-F07     TO     DE1-F07.
     MOVE      HEN-F112    TO     DE1-F112.
***2011.10.11 EN
     READ      SHTDENL1
               INVALID
               MOVE      "INV"    TO    SHTDENF-INV2-FLG
***************DISPLAY "INVALID    " UPON CONS
               NOT  INVALID
               MOVE      SPACE    TO    SHTDENF-INV2-FLG
***************DISPLAY "NOT INVALID" UPON CONS
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*                変換後売上伝票ファイル読込み                  *
****************************************************************
 JHSHENF-READ-SEC          SECTION.
*
     MOVE      "JHSHENF-READ-SEC" TO    S-NAME.
*
     READ      JHSHENF  AT  END
               MOVE     "END"     TO    END-FLG
               GO                 TO    JHSHENF-READ-EXIT
     END-READ.
*2001/02/26 NAV START*
     IF        HEN-F46  NOT =  LINK-HIDUKE
     OR        HEN-F47  NOT =  LINK-JIKAN
     OR        HEN-F01  NOT =  LINK-TORICD
               MOVE     "END"     TO    END-FLG
     END-IF.
*2001/02/26 NAV END  *
*
 JHSHENF-READ-EXIT.
     EXIT.
****************************************************************
*                当日スケジュールマスタ更新                   *
****************************************************************
 JSMDAYF-REWRITE-SEC       SECTION.
*
     MOVE      "JSMDAYF-REWRITE-SEC" TO S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O     JSMDAYF.
*
     MOVE      LINK-HIDUKE     TO   TJS-F01.
     MOVE      LINK-JIKAN      TO   TJS-F02.
     MOVE      LINK-TORICD     TO   TJS-F03.
*ファイル読込み
     READ      JSMDAYF
               INVALID
                 MOVE  "INV"   TO   JSMDAYF-INV-FLG
               NOT  INVALID
                 MOVE  SPACE   TO   JSMDAYF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF   JSMDAYF-INV-FLG  =  "INV"
          MOVE SPACE         TO  TJS-REC
          INITIALIZE             TJS-REC
          MOVE LINK-HIDUKE   TO  TJS-F01
          MOVE LINK-JIKAN    TO  TJS-F02
          MOVE LINK-TORICD   TO  TJS-F03
          MOVE "K523"        TO  TJS-F15
          WRITE TJS-REC
     ELSE
          MOVE "K523"        TO  TJS-F15
          REWRITE TJS-REC
     END-IF.
*ファイルのＣＬＯＳＥ
     CLOSE        JSMDAYF.
*
 JSMDAYF-REWRITE-EXIT.
     EXIT.
*# 2020/04/28 NAV ST Ｄ３６５連携対応
****************************************************************
*     Ｄ３６５伝票番号採番　　　　　　　                       *
****************************************************************
*D365-DEN-SEC              SECTION.
*
*    MOVE      "D365-DEN-SEC"     TO    S-NAME.
*
*    INITIALIZE                   WK-D365-DEN-PARA.
*    MOVE      "E"         TO     WK-D365-PARA-IN1.
*    MOVE      HEN-F01     TO     WK-D365-PARA-IN2.
*    CALL      "SKYD3DEN"  USING  WK-D365-PARA-IN1
*                                 WK-D365-PARA-IN2
*                                 WK-D365-PARA-OUT1
*                                 WK-D365-PARA-OUT2.
*    IF   WK-D365-PARA-OUT2  =  "1"
*         DISPLAY "## " NC"異常" "D365NO" NC"採番" " ERR]] ##"
*         MOVE    ALL "9"  TO     WK-D365-PARA-OUT1
*    END-IF.
*****DISPLAY "WK-D365-PARA-OUT1 = " WK-D365-PARA-OUT1 UPON CONS.
*
*D365-DEN-EXIT.
*    EXIT.
****************************************************************
*          ＳＵＢ商品変換テーブル読込み                        *
****************************************************************
 SUBTBLF-READ-SEC          SECTION.
*
     MOVE      "SUBTBLF-READ-SEC" TO    S-NAME.
*
     MOVE      HEN-F01     TO     SUB-F01.
     MOVE      HEN-F25     TO     SUB-F02.
     READ      SUBTBLF
               INVALID
               MOVE      "INV"    TO    SUBTBLF-INV-FLG
               MOVE      SPACE    TO    WK-SENKOU-KBN
               NOT  INVALID
               MOVE      SPACE    TO    SUBTBLF-INV-FLG
               MOVE      SUB-F19  TO    WK-SENKOU-KBN
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
****************************************************************
*          条件ファイル読込                                    *
****************************************************************
 HJYOKEN-READ-SEC          SECTION.
*
     MOVE      "HJYOKEN-READ-SEC" TO    S-NAME.
*
     MOVE      60          TO     JYO-F01.
     MOVE      "D365NOE"   TO     JYO-F02.
     READ      HJYOKEN
               INVALID
               MOVE      "INV"    TO    HJYOKEN-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*# 2020/04/28 NAV ED Ｄ３６５連携対応
*--------------------------------------------------------------*
*    LEVEL   1.1   採番　　　                      *
*--------------------------------------------------------------*
 SAIBAN           SECTION.
     MOVE     "SAIBAN"       TO     SEC-NAME.
*Ｄ３６５伝票編集
     ADD      1              TO     JD3-F04.
*最大値以上の場合、最小値をセット
     IF       JD3-F04   >  JD3-F06
              MOVE  JD3-F05  TO     JD3-F04
     END-IF.
*Ｄ３６５伝票番号作成
*****MOVE     "E"            TO     WK-D365-TYPE.
*    MOVE     HEN-F01        TO     WK-D365-TOKCD.
*    MOVE     JD3-F07        TO     WK-D365-SYSDT.
*    MOVE     WK-D365-SYSDT2 TO     WK-D365-DATE.
*    MOVE     JD3-F04        TO     WK-RENBAN.
*****MOVE     WK-RENBAN      TO     WK-D365-RENBAN.
*#2020/07/09 NAV ST
     MOVE     "E"            TO     WK-D3652-TYPE.
     MOVE     JD3-F07        TO     WK-D365-SYSDT.
     MOVE     WK-D365-SYSDT2 TO     WK-D3652-DATE.
     MOVE     JD3-F04        TO     WK-RENBAN.
     MOVE     WK-RENBAN      TO     WK-D3652-RENBAN.
     MOVE     SPACE          TO     WK-D3652-ORDER.
*#2020/07/09 NAV ED
*
  SAIBAN-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
