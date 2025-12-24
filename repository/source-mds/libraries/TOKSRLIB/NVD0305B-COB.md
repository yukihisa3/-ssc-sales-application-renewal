# NVD0305B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0305B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　Ｄ３６５連携データ作成（入荷実績）*
*    処理概要　　　　　　：　入庫ファイルより入荷実績データを　*
*    　　　　　　　　　　　　作成する。　　　　　　　　　　　　*
*    作成日／作成者　　　：　2019/12/25   ASS.TAKAHASHI        *
*    更新日／更新者　　　：　YYYY/MM/DD                        *
*    　　変更概要　　　　：                                    *
*    更新日／更新者　　　：　2020/11/09  NAV TAKAHASHI         *
*    　　変更概要　　　　：　実入荷日（未来日は処理対象×）　　*
*    更新日／更新者　　　：　2020/11/19  NAV TAKAHASHI         *
*    　　変更概要　　　　：　作業実績データ作成（ＳＦ）　　　　*
*    更新日／更新者　　　：　2021/06/08  NAV TAKAHASHI         *
*    　　変更概要　　　　：　行番号制御変更　　　　　　　　　　*
*    更新日／更新者　　　：　2021/08/10  NAV TAKAHASHI         *
*    　　変更概要　　　　：　作業実績データ作成条件追加　　　　*
*    　　　　　　　　　　：　（ＤＵＭＭＹの場合は作成しない）　*
*    更新日／更新者　　　：　2021/09/28  NAV TAKAHASHI         *
*    　　変更概要　　　　：　数量＝０の場合は連携ＤＴ作成無し　*
*    更新日／更新者　　　：　2021/11/04  NAV TAKAHASHI         *
*    　　変更概要　　　　：　入荷実績、作業実績キー情報追加　　*
*    更新日／更新者　　　：　2021/12/10  NAV INOUE             *
*    　　変更概要　　　　：　入荷実績出力時：商品変更区分に１　*
*    　　　　　　　　　　　　作業実績出力時：直送請求フラグに１*
*    更新日／更新者　　　：　2022/12/20  NAV TAKAHASHI         *
*    　　変更概要　　　　：　たねまる番号の先頭が？の時、対応　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0305B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入庫ファイル >>*************************************
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYU-F02
                                                 NYU-F03
                                                 NYU-F04
                                                 NYU-F05
                        FILE      STATUS    IS   NYU-STATUS.
****<< 発注ファイル >>*************************************
     SELECT   HACMEIF   ASSIGN    TO        DA-01-VI-HACMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HAM-F02
                                                 HAM-F03
                        FILE      STATUS    IS   HAM-STATUS.
****<< 入荷予定累積ファイル >>******************************
     SELECT   NYKRUIF   ASSIGN    TO        DA-01-VI-NYKRUIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   RUI-F01
                                                 RUI-F02
                                                 RUI-F03
                        FILE      STATUS    IS   RUI-STATUS.
****<< 入荷実績データ >>************************************
     SELECT   JISNYKF   ASSIGN    TO        DA-01-S-JISNYKF
                        FILE      STATUS    IS   JIS-STATUS.
*#2020/11/19 NAV ST
****<< 作業実績データ >>************************************
     SELECT   SNDSGJF   ASSIGN    TO        DA-01-S-SNDSGJF
                        FILE      STATUS    IS   SGJ-STATUS.
*#2020/11/19 NAV ED
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入庫ファイル >>*************************************
 FD  NYKFILF.
*###   COPY   NYKFILF
*###          DISJOINING  XXX  JOINING  NYU  AS PREFIX.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYU       PREFIX.
****<< 発注ファイル >>*************************************
 FD  HACMEIF.
*###   COPY   HACMEIF
*###          DISJOINING  XXX  JOINING  HAM  AS PREFIX.
     COPY     HACMEIF   OF        XFDLIB
              JOINING   HAM       PREFIX.
****<< 入荷予定累積ファイル >>*****************************
 FD  NYKRUIF.
*###   COPY   NYKRUIF
*###          DISJOINING  XXX  JOINING  RUI  AS PREFIX.
     COPY     NYKRUIF   OF        XFDLIB
              JOINING   RUI       PREFIX.
****<< 入荷実績データ >>************************************
 FD  JISNYKF.
*###   COPY   JISNYKF
*###          DISJOINING  XXX  JOINING  JIS  AS PREFIX.
     COPY     JISNYKF   OF        XFDLIB
              JOINING   JIS       PREFIX.
*#2020/11/19 NAV ST
****<< 作業実績データ >>***********************************
 FD  SNDSGJF.
     COPY     SNDSGJF   OF        XFDLIB
              JOINING   SGJ       PREFIX.
*#2020/11/19 NAV ED
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 NYU-STATUS           PIC  X(02).
     02 HAM-STATUS           PIC  X(02).
     02 RUI-STATUS           PIC  X(02).
     02 JIS-STATUS           PIC  X(02).
*#2020/11/19 NAV ST
     02 SGJ-STATUS           PIC  X(02).
*#2020/11/19 NAV ED
****  システム日付・時刻
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC  9(06).
     03  WK-SYSDATE          PIC  9(08).
     03  SYSHMS.
         05  WK-SYSTIME      PIC  9(06).
****  フラグ
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  HACMEIF-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  NYKRUIF-INV-FLG         PIC  X(03)  VALUE  SPACE.
*↓2021.12.10
 01  JIS-OUT-FLG             PIC  9(01)  VALUE  ZERO.
 01  SGJ-OUT-FLG             PIC  9(01)  VALUE  ZERO.
*↑2021.12.10
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
*#2020/11/19 NAV ST
     03  OUT-CNT1            PIC  9(10)  VALUE  ZERO.
*#2020/11/19 NAV ED
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILL1   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILL1  (INPUT) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** JISNYKF  (OUTPUT) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.
*#2020/11/19 NAV ST
     03  DSP-OUT1-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** SNDSGJF  (OUTPUT) =".
         05  DSP-OUT1        PIC  Z,ZZZ,ZZZ,ZZ9.
*#2020/11/19 NAV ED
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0305B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  PARA-SYUBETU            PIC  X(02).
 01  PARA-CNT                PIC  9(10).
 01  PARA-CNT1               PIC  9(10).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-SYUBETU
                                      PARA-CNT
                                      PARA-CNT1.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILL1"        TO    ERR-FL-ID.
     MOVE    NYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HACMEIF.
     MOVE   "HACMEIL1"        TO    ERR-FL-ID.
     MOVE    HAM-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKRUIF.
     MOVE   "NYKRUIL1"        TO    ERR-FL-ID.
     MOVE    RUI-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JISNYKF.
     MOVE   "JISNYKF"        TO    ERR-FL-ID.
     MOVE    JIS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*#2020/11/19 NAV ST
***
 FILEERR-SEC5           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SNDSGJF.
     MOVE   "SNDSGJF"        TO    ERR-FL-ID.
     MOVE    SGJ-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*#2020/11/19 NAV ED
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
                            UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理
************************************************************
 100-INIT-SEC           SECTION.

*#### TEST <<<<<<
*### DISPLAY " データ種別 ------- "  PARA-SYUBETU  UPON CONS.
*### DISPLAY "  " UPON CONS.
*#### TEST >>>>>>

*システム日付・時刻の取得
     ACCEPT   SYSYMD     FROM     DATE.
     ACCEPT   SYSHMS     FROM     TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-SYSDATE.

     OPEN     I-O       NYKFILF.
     OPEN     I-O       HACMEIF.
     OPEN     INPUT     NYKRUIF.
     OPEN     OUTPUT    JISNYKF.
*#2020/11/19 NAV ST
     OPEN     OUTPUT    SNDSGJF.
*#2020/11/19 NAV ED

*入庫ファイルの読込
     PERFORM  NYKFILF-READ-SEC.

 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.

*発注明細ファイル／検索
     MOVE     NYU-F02         TO   HAM-F02.      *> NAVS-NO
     MOVE     NYU-F05         TO   HAM-F03.      *> 行番号
     PERFORM  HACMEIF-READ-SEC.
     IF  HACMEIF-INV-FLG  =  "INV"
         DISPLAY "＃＃＃　発注明細ファイルなし KEY="
                  HAM-F02 "," HAM-F03   UPON  CONS
         MOVE    4000             TO    PROGRAM-STATUS
         STOP    RUN
     END-IF.

*入荷予定累積ファイル／検索
     MOVE     ZERO            TO   RUI-F01(1:3). *> NAVS-NO
     MOVE     NYU-F02         TO   RUI-F01(4:7). *> NAVS-NO
     MOVE     HAM-F21         TO   RUI-F02.      *> 伝票Ｎｏ
     MOVE     NYU-F05         TO   RUI-F03.      *> 行番号
     PERFORM  NYKRUIF-READ-SEC.
     IF  NYKRUIF-INV-FLG  =  "INV"
         DISPLAY "＃＃＃　入荷予定累積ファイルなし KEY="
                  RUI-F01 "," RUI-F02 "," RUI-F03
                                        UPON  CONS
         MOVE    4000             TO    PROGRAM-STATUS
         STOP    RUN
     END-IF.

*入荷実績データ／編集
     MOVE     SPACE               TO   JIS-REC.
     INITIALIZE                        JIS-REC.
*  伝票番号
     MOVE     RUI-F02             TO   JIS-F01.
*#2022/12/20 NAV ST　たねまる番号先頭が？の時、削除してセット
     IF  RUI-F02(1:1)  =  "?"
         MOVE  SPACE              TO   JIS-F01
         MOVE  RUI-F02(2:19)      TO   JIS-F01(1:19)
     END-IF.
*#2022/12/20 NAV ED
*  行番号
     MOVE     RUI-F03             TO   JIS-F02.
*#2021/06/08 NAV ST
     MOVE     RUI-F061            TO   JIS-F02.
*#2021/06/08 NAV ED
*  仕入先コード
     MOVE     RUI-F062            TO   JIS-F03.
     IF   RUI-F062  =  "D365SIIR"
          MOVE  SPACE             TO   JIS-F03
     END-IF.
*  発注日
     MOVE     RUI-F063            TO   JIS-F04.
*  納品日
*#2020/11/19 NAV ST　実納品日に変更
*****MOVE     RUI-F064            TO   JIS-F05.
     MOVE     NYU-F26(1:4)        TO   JIS-F05(1:4).
     MOVE     "/"                 TO   JIS-F05(5:1).
     MOVE     NYU-F26(5:2)        TO   JIS-F05(6:2).
     MOVE     "/"                 TO   JIS-F05(8:1).
     MOVE     NYU-F26(7:2)        TO   JIS-F05(9:2).
*#2020/11/19 NAV ED
*  新商品コード
     MOVE     RUI-F065            TO   JIS-F06.
*  旧商品コード
     MOVE     RUI-F066            TO   JIS-F07.
*  ＪＡＮＣＤ
     MOVE     RUI-F067            TO   JIS-F08.
*  ストックＮＯ
     MOVE     RUI-F068            TO   JIS-F09.
*  入荷数量(符号)
     IF  NYU-F01  =  51
         MOVE  "-"                TO   JIS-F10
     ELSE
         MOVE  "0"                TO   JIS-F10
     END-IF.
*  入荷数量
     COMPUTE  JIS-F11  =  NYU-F18  *  100.
*####MOVE     NYU-F18             TO   JIS-F11.
*  単価
     MOVE     RUI-F06B            TO   JIS-F12.
*####COMPUTE  JIS-F12  =  RUI-F06B *  100.
*  入荷金額(符号)
     IF  NYU-F01  =  51
         MOVE  "-"                TO   JIS-F13
     ELSE
         MOVE  "0"                TO   JIS-F13
     END-IF.
*  入荷金額
     COMPUTE  JIS-F14  =  NYU-F18 * ( RUI-F06B / 100 ).
*####COMPUTE  JIS-F14  =  NYU-F18 * RUI-F06B.
*  入庫倉庫コード
     MOVE     RUI-F06E            TO   JIS-F15.
*  入庫場所コード
     MOVE     RUI-F06F            TO   JIS-F16.
*  完納フラグ
     IF  NYU-F06  =  1
         MOVE  "1"                TO   JIS-F17
     ELSE
         MOVE  "0"                TO   JIS-F17
     END-IF.
*#2021/11/04 NAV ST
*  発注伝票ＮＯ
     MOVE     NYU-F02             TO   JIS-F18.
*  発注伝票番号
     MOVE     NYU-F05             TO   JIS-F19.
*  枝番
     MOVE     NYU-F03             TO   JIS-F20.
*#2021/11/04 NAV ED
*入荷実績データ／出力
*#2021/09/29 NAV ST 数量＝０の場合は連携しない
     IF  JIS-F11  >  ZERO
*****#2021/12/14 NAV ST 入荷実績連携（商品変更ＦＬＧ）が空白の時
         IF   NYU-F35  =  SPACE
              WRITE    JIS-REC
              ADD      1               TO   OUT-CNT
         END-IF
*********WRITE    JIS-REC
*********ADD      1               TO   OUT-CNT
*****#2021/12/14 NAV ED
*↓2021.12.10
         MOVE     1               TO   JIS-OUT-FLG
*↑2021.12.10
     END-IF.
*****WRITE    JIS-REC.
*****ADD      1                   TO   OUT-CNT.
*#2021/09/29 NAV ED 数量＝０の場合は連携しない

*↓2021.12.10 下へ移動
*入庫ファイル／更新
*    MOVE     1                   TO   NYU-F31.
*    MOVE     "1"                 TO   NYU-F92.
*    MOVE     WK-SYSDATE          TO   NYU-F93.
*#2021/09/28 NAV ST
*    MOVE     WK-SYSTIME(1:4)     TO   NYU-F90.
*#2021/09/28 NAV ED
*    REWRITE  NYU-REC.
*発注明細ファイル／更新
*    MOVE     1                   TO   HAM-F20.
*    REWRITE  HAM-REC.
*↑2021.12.10
*
*作業実績出力判定
     IF  RUI-F068  =  SPACE
*↓2021.12.10
*        GO                       TO   200-MAIN-010
         GO                       TO   200-MAIN-005
*↑2021.12.10
     ELSE   *>ストックＮＯ　ＤＵＭＭＹの場合は対象外とする
         IF  RUI-F068  =  "DUMMY "
*↓2021.12.10
*            GO                   TO   200-MAIN-010
             GO                   TO   200-MAIN-005
*↑2021.12.10
         END-IF
     END-IF.
*
*#2020/11/19 NAV ST 作業実績データ出力追加
*初期化
     MOVE     SPACE               TO   SGJ-REC.
     INITIALIZE                        SGJ-REC.
*作業実績日
     MOVE     NYU-F26(1:4)        TO   SGJ-F01(1:4).
     MOVE     "/"                 TO   SGJ-F01(5:1).
     MOVE     NYU-F26(5:2)        TO   SGJ-F01(6:2).
     MOVE     "/"                 TO   SGJ-F01(8:1).
     MOVE     NYU-F26(7:2)        TO   SGJ-F01(9:2).
*ＪＡＮＣＤ
     MOVE     RUI-F067            TO   SGJ-F04.
*ストックＮＯ
     MOVE     RUI-F068            TO   SGJ-F05.
*入庫倉庫ＣＤ
     MOVE     RUI-F06E            TO   SGJ-F06.
*入庫場所ＣＤ
     MOVE     RUI-F06F            TO   SGJ-F07.
*数量符号
     IF  NYU-F01  =  51
         MOVE "-"                 TO   SGJ-F08
     ELSE
         MOVE "0"                 TO   SGJ-F08
     END-IF.
*数量
     COMPUTE  SGJ-F09  =  NYU-F18  *  100.
*#2021/11/04 NAV ST
*  発注伝票ＮＯ
     MOVE     NYU-F02             TO   SGJ-F10.
*  発注伝票番号
     MOVE     NYU-F05             TO   SGJ-F11.
*  枝番
     MOVE     NYU-F03             TO   SGJ-F12.
*#2021/11/04 NAV ED
*作業実績データ／出力
*#2021/09/29 NAV ST
     IF  SGJ-F09  >  ZERO
*****#2021/12/14 NAV ST 作業実績連携（直送請求ＦＬＧ）が空白の時
         IF   NYU-F36  =  SPACE
              WRITE    SGJ-REC
              ADD      1               TO   OUT-CNT1
         END-IF
*********WRITE    SGJ-REC
*********ADD      1               TO   OUT-CNT1
*****#2021/12/14 NAV ED
*↓2021.12.10
         MOVE     1               TO   SGJ-OUT-FLG
*↑2021.12.10
     END-IF.
*****WRITE    SGJ-REC.
*****ADD      1                   TO   OUT-CNT1.
*#2021/09/29 NAV ED
*
*↓2021.12.10 上から移動
 200-MAIN-005.
*入庫ファイル／更新
*    ↓2021.12.10
     IF       JIS-OUT-FLG  =  1
              MOVE    "1"         TO   NYU-F35
     END-IF.
     IF       SGJ-OUT-FLG  =  1
              MOVE    "1"         TO   NYU-F36
     END-IF.
*    ↑2021.12.10
     MOVE     1                   TO   NYU-F31.
     MOVE     "1"                 TO   NYU-F92.
     MOVE     WK-SYSDATE          TO   NYU-F93.
*#2021/09/28 NAV ST
     MOVE     WK-SYSTIME(1:4)     TO   NYU-F90.
*#2021/09/28 NAV ED
     REWRITE  NYU-REC.
*発注明細ファイル／更新
     MOVE     1                   TO   HAM-F20.
     REWRITE  HAM-REC.
*    ↓2021.12.10
     MOVE     0                   TO   JIS-OUT-FLG
                                       SGJ-OUT-FLG.
*    ↑2021.12.10
*↑2021.12.10
*
 200-MAIN-010.
*次の入庫ファイルの読込
     PERFORM  NYKFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    入庫ファイルの読込処理
************************************************************
 NYKFILF-READ-SEC       SECTION.

 NYKFILF-READ-100.
     READ    NYKFILF   NEXT
        AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        NYKFILF-READ-EXT
     END-READ.

     ADD     1                   TO   IN-CNT.
     IF     IN-CNT-X  =  "500"  OR  "000"
            MOVE   IN-CNT             TO   DSP-READ
            DISPLAY  DSP-READ-AREA  UPON   CONS
     END-IF.

*計上フラグ＝１、計上済フラグ＝空白が対象
     IF     NYU-F30  =  1
       AND  NYU-F31  =  ZERO
            CONTINUE
     ELSE
            GO          TO       NYKFILF-READ-100
     END-IF.

*データ種別を判定
     IF     NYU-F91  =  PARA-SYUBETU
            CONTINUE
     ELSE
            GO          TO       NYKFILF-READ-100
     END-IF.

*実納品日を判定（未来日日付は対象がとする。）
     IF     NYU-F26  <=  WK-SYSDATE
            CONTINUE
     ELSE
            GO          TO       NYKFILF-READ-100
     END-IF.

     ADD     1                   TO   SEL-CNT.

 NYKFILF-READ-EXT.
     EXIT.
************************************************************
*      発注ファイルの読込
************************************************************
 HACMEIF-READ-SEC       SECTION.
     READ    HACMEIF
       INVALID      KEY
          MOVE      "INV"        TO   HACMEIF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   HACMEIF-INV-FLG
     END-READ.
 HACMEIF-READ-EXT.
     EXIT.
************************************************************
*      入荷予定累積ファイルの読込
************************************************************
 NYKRUIF-READ-SEC       SECTION.
     READ    NYKRUIF
       INVALID      KEY
          MOVE      "INV"        TO   NYKRUIF-INV-FLG
       NOT INVALID      KEY
          MOVE      SPACE        TO   NYKRUIF-INV-FLG
     END-READ.
 NYKRUIF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理
************************************************************
 300-END-SEC            SECTION.

     MOVE   IN-CNT               TO   DSP-IN.
     MOVE   OUT-CNT              TO   DSP-OUT.
*#2020/11/19 NAV ST
     MOVE   OUT-CNT1             TO   DSP-OUT1.
*#2020/11/19 NAV ED
     DISPLAY  DSP-IN-AREA      UPON   CONS.
     DISPLAY  DSP-OUT-AREA     UPON   CONS.
     DISPLAY  DSP-OUT1-AREA    UPON   CONS.

     MOVE   OUT-CNT              TO   PARA-CNT.
     MOVE   OUT-CNT1             TO   PARA-CNT1.
*#### TEST <<<<<<
*### DISPLAY "  " UPON CONS.
*### DISPLAY " 件数 ------------- "  PARA-CNT  UPON CONS.
*#### TEST >>>>>>


     CLOSE  NYKFILF
            HACMEIF
            NYKRUIF
*#2020/11/19 NAV ST
************JISNYKF.
            JISNYKF
            SNDSGJF.
*#2020/11/19 NAV ED
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
