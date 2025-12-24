# SBZ0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBZ0070B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　部門間在庫移動機能構築　　　      *
*    モジュール名　　　　：　振替移動実績ＩＦ作成　　　　　　  *
*    作成日／更新日　　　：　2018/01/19                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　振替移動実績Ｆを読み、ＨＧ社内振　*
*                        ：　替ＩＦファイルを作成する。　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBZ0070B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 振替移動実績ファイル >>********************************
     SELECT   FURIDOF   ASSIGN    TO        DA-01-VI-FURIDOL4
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   IDO-F86 IDO-F01
                                                 IDO-F02 IDO-F07
                        FILE      STATUS    IS   IDO-STATUS.
****<< ＨＧ社内振替ＩＦファイル >>****************************
     SELECT   FURIACOS  ASSIGN    TO        DA-01-S-FURIACOS
                        FILE      STATUS    IS   ACS-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 振替移動実績ファイル >>********************************
 FD  FURIDOF            LABEL RECORD   IS   STANDARD.
     COPY     FURIDOF   OF        XFDLIB
              JOINING   IDO       PREFIX.
****<< ＨＧ社内振替ＩＦファイル >>****************************
 FD  FURIACOS           LABEL RECORD   IS   STANDARD.
     COPY     FURIACOS  OF        XFDLIB
              JOINING   ACS       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 IDO-STATUS           PIC  X(02).
     02 ACS-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SBZ0070B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  ERR-CNT             PIC  9(07)   VALUE  0.
     03  CRT-CNT             PIC  9(07)   VALUE  0.
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON         PIC  X(04).
 01  PARA-TANCD         PIC  X(02).
 01  PARA-KENSU         PIC  9(07).
 01  PARA-KEIJYOBI      PIC  9(08).
************************************************************
 PROCEDURE              DIVISION USING  PARA-BUMON  PARA-TANCD
                                        PARA-KENSU  PARA-KEIJYOBI.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  FURIDOF.
     MOVE   "FURIDOL4"        TO    ERR-FL-ID.
     MOVE    IDO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  FURIACOS.
     MOVE   "FURIACOS"        TO    ERR-FL-ID.
     MOVE    ACS-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
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
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         I-O       FURIDOF.
     OPEN         OUTPUT    FURIACOS.
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD8.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8.
     MOVE      LINK-OUT-YMD8      TO   WK-DATE8.
*部門間移動集計ファイル読込
     PERFORM  FURIDOF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*ＨＧ社内振替ＩＦファイル作成
     PERFORM  FURIACOS-WT-SEC.
*
     PERFORM FURIDOF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    振替移動実績ファイル読込
************************************************************
 FURIDOF-READ-SEC                   SECTION.
*
     READ   FURIDOF
       AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        FURIDOF-READ-EXIT
     END-READ.
*件数カウント
     ADD     1         TO        IN-CNT.
*ＡＣＯＳ計上ＦＬＧ＝１になったら、ＰＧを終了する。
     IF  IDO-F86  =  "1"
         MOVE          "END"     TO   END-FLG
         GO            TO        FURIDOF-READ-EXIT
     END-IF.
*エラー区分＝１は対象としない。
     IF  IDO-F83  =  "1"
        ADD            1         TO   ERR-CNT
         GO            TO        FURIDOF-READ-SEC
     END-IF.
*
 FURIDOF-READ-EXIT.
     EXIT.
************************************************************
*    ＨＧ社内振替ＩＦファイル作成
************************************************************
 FURIACOS-WT-SEC                      SECTION.
*レコード初期化
     MOVE        SPACE           TO   ACS-REC.
     INITIALIZE                       ACS-REC.
*ＳＥＱ(伝票番号）
     MOVE        IDO-F01(4:5)    TO   ACS-F01.
*行番号
     MOVE        IDO-F02         TO   ACS-F02.
*入荷部門
     MOVE        IDO-F03         TO   ACS-F03.
*入荷場所
     MOVE        IDO-F04         TO   ACS-F04.
*出荷部門
     MOVE        IDO-F05         TO   ACS-F05.
*出荷場所
     MOVE        IDO-F06         TO   ACS-F06.
*振替実績日
     MOVE        IDO-F07         TO   ACS-F07.
*サカタ商品ＣＤ
     MOVE        IDO-F08         TO   ACS-F08.
*品単１
     MOVE        IDO-F09         TO   ACS-F09.
*品単２
     MOVE        IDO-F10         TO   ACS-F10.
*品単３
     MOVE        IDO-F11         TO   ACS-F11.
*ストック番号
     MOVE        SPACE           TO   ACS-F12.
*数量符号／数量
     IF  IDO-F12  <  ZERO
         MOVE    "1"             TO   ACS-F13
     ELSE
         MOVE    "0"             TO   ACS-F13
     END-IF.
*数量
     COMPUTE  ACS-F14  = IDO-F12 *  1000.
*単価符号
     IF  IDO-F13  <  ZERO
         MOVE    "1"             TO   ACS-F15
     ELSE
         MOVE    "0"             TO   ACS-F15
     END-IF.
*単価
     COMPUTE  ACS-F16  = IDO-F13 *  1000.
*備考
     MOVE        IDO-F14         TO   ACS-F17.
*ＨＧ振替実績ＩＦファイル更新
     WRITE  ACS-REC.
     ADD               1       TO      CRT-CNT.
*振替移動実績ファイル更新
     MOVE   "1"                TO      IDO-F86.
     MOVE   WK-DATE8           TO      IDO-F87.
     MOVE   WK-TIME-HM         TO      IDO-F88.
     MOVE   WK-DATE8           TO      IDO-F96.
     MOVE   WK-TIME-HM         TO      IDO-F97.
     MOVE   PARA-BUMON         TO      IDO-F98.
     MOVE   PARA-TANCD         TO      IDO-F99.
     REWRITE  IDO-REC.
*
 FURIACOS-WT-EXIT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     CLOSE             FURIDOF  FURIACOS.
*
     DISPLAY "* FURIDOF (INPUT)  = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* FURIKAEBI ERR    = " ERR-CNT  " *"  UPON CONS.
     DISPLAY "* FURIACOS(CRT   ) = " CRT-CNT  " *"  UPON CONS.
*パラメタＯＵＴ
     MOVE     CRT-CNT           TO   PARA-KENSU.
     MOVE     WK-DATE8          TO   PARA-KEIJYOBI.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
