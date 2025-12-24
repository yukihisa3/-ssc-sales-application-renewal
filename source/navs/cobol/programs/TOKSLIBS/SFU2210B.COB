****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ストックＮＯ外しデータ作成　　　　*
*    モジュール名　　　　：　ストックＮＯ外しデータ作成　　　　*
*    作成日／更新日　　　：　07/01/05                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ストックＮＯ外しデータを読み、作業*
*                          実績ファイルデータを作成する。      *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU2210B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 作業実績ファイル >>************************************
     SELECT   SGYFILF   ASSIGN    TO        DA-01-VI-SGYFILL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SGY-F01
                                            SGY-F02
                        FILE      STATUS    IS   SGY-STATUS.
****<< ストックＮＯデータ >>*********************************
     SELECT   SUTOKUF   ASSIGN    TO        DA-01-S-SUTOKUF
                        FILE      STATUS    IS   SHO-STATUS.
****<< 条件ファイル >>**************************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 作業実績ファイル  >>************************************
 FD  SGYFILF            LABEL     RECORD     IS  STANDARD.
     COPY     SGYFILF   OF   XFDLIB    JOINING   SGY  AS   PREFIX.
****<< ストックＮＯデータ >>***********************************
 FD  SUTOKUF            BLOCK     CONTAINS   1   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     FURIKAF   OF   XFDLIB    JOINING   SHO  AS   PREFIX.
*****<< 条件ファイル >>*****************************************
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 SGY-STATUS           PIC  X(02).
     02 SHO-STATUS           PIC  X(02).
     02 JYO-ST1              PIC  X(02).
     02 JYO-ST2              PIC  X(04).
*システム日付の編集
 01  SYS-DATE          PIC 9(06).
 01  SYS-DATEW         PIC 9(08).
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(8).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU2210B".
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
 01  ZAMZAIF-END             PIC  X(03)  VALUE  SPACE.
 01  INV-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  IN-CNT              PIC  9(07)   VALUE  0.
     03  IN-CNT1             PIC  9(07)   VALUE  0.
     03  OUT-CNT1            PIC  9(07)   VALUE  0.
     03  OUT-CNT2            PIC  9(07)   VALUE  0.
     03  GYO-CNT             PIC  9(02)   VALUE  1.
     03  ST-DENNO            PIC  9(07)   VALUE  0.
     03  ED-DENNO            PIC  9(07)   VALUE  0.
 01  WK-DENNO                PIC  9(07)   VALUE  0.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
************************************************************
 PROCEDURE             DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SGYFILF.
     MOVE   "SGYFILF "        TO    ERR-FL-ID.
     MOVE    SGY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP    RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SUTOKUF.
     MOVE   "SUTOKUF "        TO    ERR-FL-ID.
     MOVE    SHO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP    RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE   "HJYOKEN "        TO    ERR-FL-ID.
     MOVE    JYO-ST1          TO    ERR-STCD.
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
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     SUTOKUF.
     OPEN         I-O       HJYOKEN.
     OPEN         I-O       SGYFILF.
*
     ACCEPT      SYS-DATE    FROM  DATE.
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
     MOVE      LINK-OUT-YMD8      TO   SYS-DATEW.
*    ストックＮＯデータ読込み
     PERFORM  SUTOKUF-READ-SEC.
*
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*    出荷データ作成
     MOVE  SPACE          TO   SGY-REC.
     INITIALIZE                SGY-REC.
*    作業番号採番
*伝票_採番処理
     MOVE     "62"                TO   JYO-F01.
     MOVE     SPACE               TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG        =    1
              DISPLAY   "HJYOKEN INV KEY=62"  UPON CONS
              STOP      RUN
     END-IF.
     MOVE     JYO-F04             TO   SGY-F01.
*
     IF       IN-CNT1  =  1
              MOVE  SGY-F01       TO   ST-DENNO
     END-IF.
*
     IF       SGY-F01    =    9999999
              MOVE       1       TO   JYO-F04
     ELSE
              ADD        1       TO   JYO-F04
     END-IF.
     REWRITE  JYO-REC.
*行番号
     MOVE     1                  TO   SGY-F02.
*作業区分
     MOVE     "58"               TO   SGY-F03.
*作業場所
     MOVE     SHO-F17            TO   SGY-F04.
*作業完成日
     MOVE     SHO-F09            TO   SGY-F05.
*入出庫区分
     MOVE     "2"                TO   SGY-F06.
*ストックＮＯ
     MOVE     SHO-F24            TO   SGY-F07.
*商品ＣＤ
     MOVE     SHO-F10            TO   SGY-F08.
*品単ＣＤ
     MOVE     SHO-F11            TO   SGY-F09.
*_番
*数量
*****MOVE     SHO-F13            TO   SGY-F11.
     COMPUTE  SGY-F11 = SHO-F13  *  2.
*備考
*計上フラグ
*担当者
     MOVE     "01"               TO   SGY-F14
*未出庫フラグ
*移動未出庫数
*移動引当済数
*予備
*部門ＣＤ
     MOVE     SHO-F01            TO   SGY-F95.
*登録日
     MOVE     SYS-DATEW          TO   SGY-F98.
*出荷レコード出力
     WRITE    SGY-REC.
     ADD      1                  TO   OUT-CNT1.
*入庫レコード処理
     MOVE     2                  TO   SGY-F02.
*入出庫区分
     MOVE     "1"                TO   SGY-F06.
*ストックＮＯ初期化
     MOVE     SPACE              TO   SGY-F07.
*入庫レコード出力
     WRITE    SGY-REC.
     ADD      1                  TO   OUT-CNT2.
*
     PERFORM SUTOKUF-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            在庫マスタの読込処理
************************************************************
 SUTOKUF-READ-SEC                   SECTION.
*
     READ    SUTOKUF
        NEXT   AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        SUTOKUF-READ-EXT
     END-READ.
*
     ADD     1         TO        IN-CNT.
     IF   IN-CNT(5:3) = "500" OR "000"
          DISPLAY "READ-CNT = " IN-CNT  UPON CONS
     END-IF.
*
     IF      SHO-F24  =  SPACE
             GO        TO        SUTOKUF-READ-SEC
     END-IF.
     ADD     1         TO        IN-CNT1.
*
 SUTOKUF-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
*
     MOVE  SGY-F01       TO   ED-DENNO.
*
     CLOSE             SUTOKUF  SGYFILF  HJYOKEN.
*
     DISPLAY "* SUTOKUF (INPUT) = " IN-CNT   " *"  UPON CONS.
     DISPLAY "* SYUKO-DT(OUTPUT)= " OUT-CNT1 " *"  UPON CONS.
     DISPLAY "* NYUKO-DT(OUTPUT)= " OUT-CNT2 " *"  UPON CONS.
     DISPLAY NC"開始伝票" " = " ST-DENNO     " *"  UPON CONS.
     DISPLAY NC"終了伝票" " = " ED-DENNO     " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-RD-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 JYO-RD-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
