****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　入出庫計上　　　　　              *
*    作成日／更新日　　　：　93/05/13                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　計上データを出力する　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            ZDA0130B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入出庫ファイル >>************************************
     SELECT   NYUS      ASSIGN    TO        DA-01-VI-ZNYUSDT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
*********************   RECORD    KEY       IS   NYU-F01
                        RECORD    KEY       IS
                                                 NYU-F02
                                                 NYU-F03
                        FILE      STATUS    IS   NYU-STATUS.
****<< 作業実績ファイル >>**********************************
     SELECT   SGYO      ASSIGN    TO        DA-01-VI-ZSGYODT1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SGY-F01
                                                 SGY-F02
                        FILE      STATUS    IS   SGY-STATUS.
****<< 計上データ >>****************************************
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        FILE      STATUS    IS   TOK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入出庫ファイル >>***********************************
 FD  NYUS.
     COPY     ZNYUSDT   OF        XFDLIB
              JOINING   NYU       PREFIX.
****<< 作業実績ファイル >>*********************************
 FD  SGYO.
     COPY     ZSGYODT   OF        XFDLIB
              JOINING   SGY       PREFIX.
****<< 計上データ >>***************************************
 FD  TOKU.
     COPY    TOKU.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 NYU-STATUS           PIC  X(02).
     02 SGY-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
****  フラグ                  ****
 01  EOF-FLG1                PIC  X(03)  VALUE  SPACE.
 01  EOF-FLG2                PIC  X(03)  VALUE  SPACE.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(6).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
 01  SYSTEM-HIZUKE2.
     02  SYSYMD2             PIC  9(8).
     02  SYSYMD2-R           REDEFINES SYSYMD2.
       03  SYS-YY2           PIC  9999.
       03  SYS-MM2           PIC  99.
       03  SYS-DD2           PIC  99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "ZDA0130B".
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
************************************************************
 PROCEDURE              DIVISION.
************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NYUS.
     MOVE   "ZNYUSDT "        TO    ERR-FL-ID.
     MOVE    NYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SGYO.
     MOVE   "ZSGYODT "        TO    ERR-FL-ID.
     MOVE    SGY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TOKU.
     MOVE   "TOKU    "        TO    ERR-FL-ID.
     MOVE    TOK-STATUS       TO    ERR-STCD.
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
             UNTIL     EOF-FLG2  =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*      １００   初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
     OPEN         I-O       NYUS.
     OPEN         I-O       SGYO.
     OPEN         EXTEND    TOKU.
*       システム日付の取得
     ACCEPT       SYSYMD    FROM     DATE.
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
     MOVE      LINK-OUT-YMD       TO   SYSYMD2.
*       入庫ファイルの読込
     PERFORM           NYUS-READ.
 100-INIT-END.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
     PERFORM      210-NYUS-SHORI
        UNTIL     EOF-FLG1  =    "END".
*
     MOVE    SPACE               TO   EOF-FLG1.
*
     PERFORM      SGYO-READ.
     PERFORM      220-SGYO-SHORI
        UNTIL     EOF-FLG2  =    "END".
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*      210       入出庫ファイルの処理                      *
************************************************************
 210-NYUS-SHORI                  SECTION.
     MOVE    SPACE               TO   TOK0-REC.
     INITIALIZE                       TOK0-REC.
*    部門
*## 1999/12/20 NAV
*****MOVE    290                 TO   TO0-F01.
     MOVE    2900                TO   TO0-F01.
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    ZERO                TO   TO0-F03(1:2).
     MOVE    NYU-F02             TO   TO0-F03(3:7).
*    行_
     MOVE    NYU-F03             TO   TO0-F04.
*    出荷日
*## 1999/12/20 NAV
*****MOVE    NYU-F15(3:6)        TO   TO0-F09
*****                                 TO0-F10.
     MOVE    NYU-F15             TO   TO0-F09
                                      TO0-F10.
*   商品コード
     MOVE    NYU-F05             TO   TO0-F11.
*   品単
     MOVE    NYU-F06             TO   TO0-F12.
*   ストック_
     MOVE    NYU-F09             TO   TO0-F13.
*   数量
     MOVE    NYU-F12             TO   TO0-F15.
*   場所
     MOVE    NYU-F10             TO   TO0-F18.
*   入出庫区分
     MOVE    "2"                 TO   TO0-F19.
*   作業明細区分
     MOVE    NYU-F04             TO   TO0-F21.
*   備考
     MOVE    NYU-F13             TO   TO0-F22.
*   処理日
*## 1999/12/20 NAV
*****MOVE    SYSYMD              TO   TO0-F27.
     MOVE    SYSYMD2             TO   TO0-F27.
*   __
     MOVE    NYU-F07             TO   TO0-F32.
*担当者
     MOVE    NYU-F16             TO   TO0-F25.
*
     IF      NYU-F10   NOT  =    SPACE
             WRITE             TOK-REC
     END-IF.
*------------------  ２件目の出力  ----------------**
*   場所
     MOVE    NYU-F11             TO   TO0-F18.
*   入出庫区分
     MOVE    "1"                 TO   TO0-F19.
*   __
     MOVE    NYU-F08             TO   TO0-F32.
*
     IF      NYU-F01        =    32
             MOVE      31   TO   TO0-F02
     END-IF.
*
     IF      NYU-F11   NOT  =    SPACE
             WRITE             TOK-REC
     END-IF.
*
     MOVE    1                   TO   NYU-F14.
     REWRITE                     NYU-REC.
*
     PERFORM      NYUS-READ.
 210-NYUS-SHORI-EXT.
     EXIT.
************************************************************
*      220       作業実績ファイルの処理                    *
************************************************************
 220-SGYO-SHORI                  SECTION.
     MOVE    SPACE               TO   TOK0-REC.
     INITIALIZE                       TOK0-REC.
*    部門
*## 1999/12/20 NAV
*****MOVE    290                 TO   TO0-F01.
     MOVE    2900                TO   TO0-F01.
*    伝票区分
     MOVE    30                  TO   TO0-F02.
*    伝票_
     MOVE    ZERO                TO   TO0-F03(1:2).
     MOVE    SGY-F01             TO   TO0-F03(3:7).
*    行_
     MOVE    SGY-F02             TO   TO0-F04.
*    出荷日
*## 1999/12/20 NAV
*****MOVE    SGY-F05(3:6)        TO   TO0-F09
*****                                 TO0-F10.
     MOVE    SGY-F05             TO   TO0-F09
                                      TO0-F10.
*   商品コード
     MOVE    SGY-F08             TO   TO0-F11.
*   品単
     MOVE    SGY-F09             TO   TO0-F12.
*   ストック_
     MOVE    SGY-F07             TO   TO0-F13.
*   数量
     MOVE    SGY-F11             TO   TO0-F15.
*   場所
     MOVE    SGY-F04             TO   TO0-F18.
*   入出庫区分
     MOVE    SGY-F06             TO   TO0-F19.
*
*   作業明細区分
     MOVE    SGY-F03             TO   TO0-F21.
*   備考
     MOVE    SGY-F12             TO   TO0-F22.
*   処理日
*## 1999/12/20 NAV
*****MOVE    SYSYMD              TO   TO0-F27.
     MOVE    SYSYMD2             TO   TO0-F27.
*   __
     MOVE    SGY-F10             TO   TO0-F32.
     MOVE    SGY-F14             TO   TO0-F25.
*
     WRITE                       TOK-REC.
*
     MOVE    1                   TO   SGY-F13.
     REWRITE                     SGY-REC.
*
     PERFORM      SGYO-READ.
 220-SGYO-SHORI-EXT.
     EXIT.
************************************************************
*      9000      入出庫ファイルの読込処理                  *
************************************************************
 NYUS-READ              SECTION.
     READ    NYUS      NEXT
        AT   END
             MOVE      "END"     TO   EOF-FLG1
             GO        TO        NYUS-READ-EXT
     END-READ.
*
     IF      NYU-F04   =         "01"
             GO        TO        NYUS-READ
     END-IF.
*
     IF      NYU-F14   NOT  =    ZERO
             GO        TO        NYUS-READ
     END-IF.
*
     IF      NYU-F96   NOT  =    SPACE
             GO        TO        NYUS-READ
     END-IF.
     IF      NYU-F10   =    SPACE     AND
             NYU-F11   =    SPACE
             GO        TO        NYUS-READ
     END-IF.
 NYUS-READ-EXT.
     EXIT.
************************************************************
*      9100      作業実績ファイル読込処理                  *
************************************************************
 SGYO-READ              SECTION.
     READ    SGYO      NEXT
        AT   END
             MOVE      "END"     TO   EOF-FLG2
             GO        TO        SGYO-READ-EXT
     END-READ.
*
     IF      SGY-F13   NOT  =    ZERO
             GO        TO        SGYO-READ
     END-IF.
*
     IF      SGY-F97   NOT  =    ZERO
             GO        TO        SGYO-READ
     END-IF.
 SGYO-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC            SECTION.
     CLOSE        NYUS      SGYO      TOKU.
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
