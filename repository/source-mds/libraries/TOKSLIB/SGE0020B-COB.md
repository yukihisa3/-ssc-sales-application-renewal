# SGE0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SGE0020B.COB`

## ソースコード

```cobol
***********************************************************
*   顧客名　　    ：   _サカタのタネ殿                   *
*   システム名    ：   在庫管理システム                   *
*   サブシステム名：   月次更新　                         *
*   プログラム名　：   最終月次日更新                     *
*   プログラムID  ：   SGE0020B                           *
*   作成者        ：   ＮＡＶ　                           *
*   作成日        ：   1993.05.05                         *
*   更新日        ：                                      *
***********************************************************
*
 IDENTIFICATION                      DIVISION.
 PROGRAM-ID.               SGE0020B.
 ENVIRONMENT                         DIVISION.
 CONFIGURATION             SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS        CONS.
*
 INPUT-OUTPUT              SECTION.
 FILE-CONTROL.
***************************************************************
*    条件ファイル
***************************************************************
     SELECT      HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       JYO-F01   JYO-F02
                           STATUS    IS        JYO-STS.
****************************************************************
 DATA                      DIVISION.
****************************************************************
 FILE                      SECTION.
***************************************************************
*    条件ファイル
***************************************************************
 FD  HJYOKEN.
     COPY        HJYOKEN   OF        XFDLIB
     JOINING     JYO       AS        PREFIX.
****************************************************************
*            WORKING-STORAGE        SECTION                    *
****************************************************************
 WORKING-STORAGE           SECTION.
 01  WK-AREA.
     03  END-FLG           PIC 9(01) VALUE     ZERO.
 01  WK-HIZUKEN            PIC 9(08).
 01  WK-HIZUKE   REDEFINES WK-HIZUKEN.
     03  WK-HIZUKE2        PIC 9(02).
     03  WK-HIZUKE6        PIC 9(06).
 01  WK-HIZUKEXN           PIC 9(04).
 01  WK-HIZUKEX  REDEFINES WK-HIZUKEXN.
     03  WK-HIZUKE2X       PIC 9(02).
     03  WK-HIZUKE2XX      PIC 9(02).
 01  JYO-STS               PIC X(02).
 01  WK-JYO-F05            PIC 9(09).
 01  WK-JYO-F05-R          REDEFINES     WK-JYO-F05.
     03  WK-JYO-F051       PIC 9(07).
     03  WK-JYO-F052       PIC 9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*
****************************************************************
 PROCEDURE                           DIVISION.
****************************************************************
 DECLARATIVES.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY    "#ABEND#"  UPON      CONS.
     DISPLAY     JYO-STS   UPON      CONS.
     MOVE       "4000"     TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*
****************************************************************
 PROC-SEC                  SECTION.
*
     DISPLAY     "***  SGE0020B START  ***"     UPON CONS.
*
     OPEN        I-O       HJYOKEN.
*
*    条件ファイルを読み西暦変換年を取得する
*****
*****MOVE        57        TO        JYO-F01.
*****MOVE        SPACE     TO        JYO-F02.
*****READ    HJYOKEN
*****  INVALID
*****    MOVE    19        TO        WK-HIZUKE2
*****  NOT INVALID
*****    MOVE    JYO-F04   TO        WK-HIZUKEXN
*****    MOVE    WK-HIZUKE2X   TO    WK-HIZUKE2
*****END-READ.
*
*    条件ファイルの最終更新日を本日の日付で更新する
     ACCEPT      WK-HIZUKE6     FROM DATE.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     WK-HIZUKE6 TO       LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE   LINK-OUT-YMD8  TO        WK-HIZUKEN.
     MOVE        99        TO        JYO-F01.
     MOVE        "ZAI"     TO        JYO-F02.
*
     READ        HJYOKEN.
     MOVE        WK-HIZUKEN     TO   JYO-F04.
     ADD         1              TO   JYO-F05.
     MOVE        JYO-F05        TO   WK-JYO-F05.
     IF          WK-JYO-F052    >    12
                 MOVE  1        TO   WK-JYO-F052
                 ADD   1        TO   WK-JYO-F051
     END-IF.
     MOVE        WK-JYO-F05     TO   JYO-F05.
*
*
     REWRITE     JYO-REC.
*
     DISPLAY     "***  SGE0020B END    ***"     UPON CONS.
*
     CLOSE                 HJYOKEN.
 PROC-EXIT.
     EXIT.
*

```
