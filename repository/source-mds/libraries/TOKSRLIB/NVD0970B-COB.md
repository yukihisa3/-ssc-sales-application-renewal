# NVD0970B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0970B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷実績送信ＦＬＧ解除　　　　　　*
*    処理概要　　　　　　：　入庫ファイルの送信情報を解除する　*
*    作成日／作成者　　　：　2023/05/24                        *
*    更新日／更新者　　　：　YYYY/MM/DD                        *
*    　　変更概要　　　　：                                    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NVD0970B.
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
     SELECT   NYKFILF   ASSIGN    TO        DA-01-VI-NYKFILL5
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYK-F93
                                                 NYK-F90
                        WITH      DUPLICATES
                        FILE      STATUS    IS   NYK-STATUS.
****<< 入庫ファイルＢＫ >>************************************
     SELECT   NYKFILW   ASSIGN    TO        DA-01-S-NYKFILW
                        FILE      STATUS    IS   NYW-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入庫ファイル >>*************************************
 FD  NYKFILF.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYK       PREFIX.
****<< 入庫ファイルＢＫ >>************************************
 FD  NYKFILW.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYW       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報
 01  STATUS-AREA.
     02 NYK-STATUS           PIC  X(02).
     02 NYW-STATUS           PIC  X(02).
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
****  カウント
 01  CNT-AREA.
     03  IN-CNT              PIC  9(10)  VALUE  ZERO.
     03  IN-CNT-R            REDEFINES  IN-CNT.
         05  FILLER          PIC  X(07).
         05  IN-CNT-X        PIC  X(03).
     03  SEL-CNT             PIC  9(10)  VALUE  ZERO.
     03  OUT-CNT             PIC  9(10)  VALUE  ZERO.
****  処理件数表示
 01  DSP-CNT-AREA.
     03  DSP-READ-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILL5   (READ) =".
         05  DSP-READ        PIC  Z,ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(15) VALUE
         " * 処理中]]".
     03  DSP-IN-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILL5 (UPDATE) =".
         05  DSP-IN          PIC  Z,ZZZ,ZZZ,ZZ9.
     03  DSP-OUT-AREA.
         05  FILLER          PIC  X(23) VALUE
         "*** NYKFILW  (OUTPUT) =".
         05  DSP-OUT         PIC  Z,ZZZ,ZZZ,ZZ9.

**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NVD0970B".
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
 01  PARA-JDATE            PIC  9(08).
 01  PARA-JTIME            PIC  X(04).
*01  PARA-CNT                PIC  9(10).
****************************************************************
 PROCEDURE                   DIVISION
                                USING PARA-JDATE
                                      PARA-JTIME.
*                                     PARA-CNT.
****************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILF.
     MOVE   "NYKFILL5"        TO    ERR-FL-ID.
     MOVE    NYK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYKFILW.
     MOVE   "NYKFILW"         TO    ERR-FL-ID.
     MOVE    NYW-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
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
*### DISPLAY " データ種別 ------- "  PARA-JDATE  UPON CONS.
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
     OPEN     OUTPUT    NYKFILW.
*T
     DISPLAY "PARA-JDATE=" PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME=" PARA-JTIME UPON CONS.
*T
*
*入庫ファイルのＳＴＡＲＴ
     PERFORM  NYKFILF-START-SEC.
     IF       END-FLG =  "END"
              DISPLAY NC"＃入荷実績データなし＃"    UPON CONS
              DISPLAY NC"　　計上日付＝" PARA-JDATE UPON CONS
              DISPLAY NC"　　計上時刻＝" PARA-JTIME UPON CONS
              GO      TO     100-INIT-END
     END-IF.
*入庫ファイルの読込
     PERFORM  NYKFILF-READ-SEC.
     IF       END-FLG =  "END"
              DISPLAY NC"＃入荷実績データなし＃"    UPON CONS
              DISPLAY NC"　　計上日付＝" PARA-JDATE UPON CONS
              DISPLAY NC"　　計上時刻＝" PARA-JTIME UPON CONS
              GO      TO     100-INIT-END
     END-IF.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理
************************************************************
 200-MAIN-SEC           SECTION.
*
*入庫ファイルＢＫに退避
     MOVE     NYK-REC             TO   NYW-REC.
     WRITE    NYW-REC.
     ADD      1                   TO   OUT-CNT.
*入庫ファイル更新
*  計上済フラグ
     MOVE     ZERO                TO   NYK-F31.
*  Ｄ３６５計上区分
     MOVE     SPACE               TO   NYK-F92.
*  Ｄ３６５計上日
     MOVE     SPACE               TO   NYK-F93.
*  計上済フラグ
     MOVE     SPACE               TO   NYK-F35.
*  相手倉庫コード
     MOVE     SPACE               TO   NYK-F36.
*
     REWRITE  NYK-REC.
*
*次の入庫ファイルの読込
     PERFORM  NYKFILF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*    入庫ファイルのＳＴＡＲＴ
************************************************************
 NYKFILF-START-SEC       SECTION.

 NYKFILF-START-100.
     MOVE    PARA-JDATE          TO   NYK-F93.
     MOVE    PARA-JTIME          TO   NYK-F90.
     IF      PARA-JTIME = "0000"
             MOVE    SPACE       TO   NYK-F90
     END-IF.
     START   NYKFILF
        INVALID
             MOVE      "END"     TO   END-FLG
             GO        TO        NYKFILF-START-EXT
     END-START.
*
 NYKFILF-START-EXT.
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
*
*計上日判定
     IF     NYK-F93  =  PARA-JDATE
            CONTINUE
     ELSE
*T
*           DISPLAY "NYK-F93=" NYK-F93 UPON CONS
*T
            GO          TO       NYKFILF-READ-100
     END-IF.
*計上時刻判定
*T
*    DISPLAY "START" UPON CONS.
*    DISPLAY "NYK-F93="  NYK-F93 UPON CONS.
*    DISPLAY "NYK-F90="  NYK-F90 UPON CONS.
*T
     IF   ( PARA-JTIME  =  SPACE   ) OR
          ( PARA-JTIME  =  ZERO    ) OR
          ( NYK-F90  =  PARA-JTIME )
            CONTINUE
     ELSE
*T
*           DISPLAY "NYK-F90=" NYK-F90 UPON CONS
*T
            GO          TO       NYKFILF-READ-100
     END-IF.
*
 NYKFILF-READ-200.
     ADD     1                   TO   SEL-CNT.

 NYKFILF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理
************************************************************
 300-END-SEC            SECTION.
*
     MOVE   IN-CNT               TO   DSP-READ.
     MOVE   SEL-CNT              TO   DSP-IN.
     MOVE   OUT-CNT              TO   DSP-OUT.
     DISPLAY  DSP-READ-AREA      UPON   CONS.
     DISPLAY  DSP-IN-AREA      UPON   CONS.
     DISPLAY  DSP-OUT-AREA     UPON   CONS.
*
*    MOVE   OUT-CNT              TO   PARA-CNT.
*
*#### TEST <<<<<<
*### DISPLAY "  " UPON CONS.
*### DISPLAY " 件数 ------------- "  PARA-CNT  UPON CONS.
*#### TEST >>>>>>
*
*
     CLOSE  NYKFILF
            NYKFILW.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
