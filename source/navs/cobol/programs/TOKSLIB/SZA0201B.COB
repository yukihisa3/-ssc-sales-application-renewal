****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　入出庫計上　　　　　              *
*    作成日／更新日　　　：　2000/05/19                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　作業実績／入出庫データを計上する。*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZA0201B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 入出庫ファイル >>************************************
     SELECT   NYSFILF   ASSIGN    TO        DA-01-VI-NYSFILF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   NYU-F02
                                                 NYU-F03
                        FILE      STATUS    IS   NYU-STATUS.
****<< 作業実績ファイル >>**********************************
     SELECT   SGYFILF   ASSIGN    TO        DA-01-VI-SGYFILF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   SGY-F01
                                                 SGY-F02
                        FILE      STATUS    IS   SGY-STATUS.
****<< 条件ファイル >>**************************************
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
****<< 計上データ >>****************************************
     SELECT   TOKU      ASSIGN    TO        DA-01-S-TOKU
                        FILE      STATUS    IS   TOK-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 入出庫ファイル >>***********************************
 FD  NYSFILF.
     COPY     NYSFILF   OF        XFDLIB
              JOINING   NYU       PREFIX.
****<< 作業実績ファイル >>*********************************
 FD  SGYFILF.
     COPY     SGYFILF   OF        XFDLIB
              JOINING   SGY       PREFIX.
****<<  条件ファイル　　>>*********************************
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
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
     02 JYO-STATUS           PIC  X(02).
     02 TOK-STATUS           PIC  X(02).
****  フラグ                  ****
 01  EOF-FLG1                PIC  X(03)  VALUE  SPACE.
 01  EOF-FLG2                PIC  X(03)  VALUE  SPACE.
****  部門                    ****
 01  WK-BUMON                PIC  9(04)  VALUE  ZERO.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SZA0201B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*****倉庫＋部門保存
 01  WK-SOKCD                     PIC  X(02)  VALUE  SPACE.
 01  WK-BUMON-CD                  PIC  9(04)  VALUE  ZERO.
 01  WK-BUMON-CD1                 PIC  9(04)  VALUE  ZERO.
 01  WK-BUMON-CD2                 PIC  9(04)  VALUE  ZERO.
 01  HJYOKEN-INV-FLG              PIC  X(03)  VALUE  SPACE.
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
                   PROCEDURE  NYSFILF.
     MOVE   "NYSFILF "        TO    ERR-FL-ID.
     MOVE    NYU-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  SGYFILF.
     MOVE   "SGYFILF "        TO    ERR-FL-ID.
     MOVE    SGY-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HJYOKEN.
     MOVE     "HJYOKEN"       TO   ERR-FL-ID.
     MOVE     JYO-STATUS      TO   ERR-STCD.
     DISPLAY  MSG-ABEND1      UPON   CONS.
     DISPLAY  MSG-ABEND2      UPON   CONS.
     MOVE     4000            TO   PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC4           SECTION.
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
*    ファイルＯＰＥＮ
     OPEN         INPUT     HJYOKEN.
     OPEN         I-O       NYSFILF.
     OPEN         I-O       SGYFILF.
     OPEN         EXTEND    TOKU.
*    条件ファイル索引（部門コード取得）
*****　更新日を取得する　****
     MOVE    99              TO   JYO-F01.
     MOVE    "BUMON"         TO   JYO-F02.
     READ    HJYOKEN
             INVALID
             DISPLAY "HJYOKEN BUMON INVALID" JYO-F01 " - " JYO-F02
                      UPON CONS
                      STOP RUN
             NOT  INVALID
             MOVE     JYO-F04     TO   WK-BUMON
     END-READ.
*    システム日付の取得
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
*    入出庫データ計上処理
     PERFORM      210-NYUS-SHORI
        UNTIL     EOF-FLG1  =    "END".
*
     MOVE    SPACE               TO   EOF-FLG1.
*    作業実績データ計上処理
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
*****MOVE    WK-BUMON            TO   TO0-F01.
*****出庫場所部門取得
     MOVE    NYU-F10             TO   WK-SOKCD.
     PERFORM HJYOKEN-READ-SEC.
     IF      HJYOKEN-INV-FLG = "INV"
             GO                  TO   210-NYUS-010
     ELSE
             MOVE  WK-BUMON-CD   TO   WK-BUMON-CD1
     END-IF.
*    部門
     MOVE    WK-BUMON-CD1        TO   TO0-F01.
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    ZERO                TO   TO0-F03(1:2).
     MOVE    NYU-F02             TO   TO0-F03(3:7).
*    行_
     MOVE    NYU-F03             TO   TO0-F04.
*    出荷日
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
     MOVE    SYSYMD2             TO   TO0-F27.
*   __
     MOVE    NYU-F07             TO   TO0-F32.
*担当者
     MOVE    NYU-F16             TO   TO0-F25.
*
     IF      NYU-F10   NOT  =    SPACE
             WRITE             TOK-REC
     END-IF.
 210-NYUS-010.
*------------------  ２件目の出力  ----------------**
*****入庫場所部門取得
     MOVE    NYU-F11             TO   WK-SOKCD.
     PERFORM HJYOKEN-READ-SEC.
     IF      HJYOKEN-INV-FLG = "INV"
             GO                  TO   210-NYUS-020
     ELSE
             MOVE  WK-BUMON-CD   TO   WK-BUMON-CD2
     END-IF.
*    部門
     MOVE    WK-BUMON-CD2        TO   TO0-F01.
*   場所
     MOVE    NYU-F11             TO   TO0-F18.
*   入出庫区分
     MOVE    "1"                 TO   TO0-F19.
*   __
     MOVE    NYU-F08             TO   TO0-F32.
*##2003/08/04 NAV START 修正　項目追加##
*    伝票区分
     MOVE    NYU-F01             TO   TO0-F02.
*    伝票_
     MOVE    ZERO                TO   TO0-F03(1:2).
     MOVE    NYU-F02             TO   TO0-F03(3:7).
*    行_
     MOVE    NYU-F03             TO   TO0-F04.
*    出荷日
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
*   作業明細区分
     MOVE    NYU-F04             TO   TO0-F21.
*   備考
     MOVE    NYU-F13             TO   TO0-F22.
*   処理日
     MOVE    SYSYMD2             TO   TO0-F27.
*担当者
     MOVE    NYU-F16             TO   TO0-F25.
*   伝票区分
     IF      NYU-F01        =    32
             MOVE      31   TO   TO0-F02
     END-IF.
*   数量チェック後、計上ファイル追加
     IF      NYU-F11   NOT  =    SPACE
             WRITE             TOK-REC
     END-IF.
 210-NYUS-020.
*   計上フラグ更新（計上済）
     MOVE    1                   TO   NYU-F14.
     REWRITE                     NYU-REC.
*   入出庫ファイル読込み
     PERFORM      NYUS-READ.
 210-NYUS-SHORI-EXT.
     EXIT.
************************************************************
*      220       作業実績ファイルの処理                    *
************************************************************
 220-SGYO-SHORI                  SECTION.
     MOVE    SPACE               TO   TOK0-REC.
     INITIALIZE                       TOK0-REC.
*    部門（対象部門が数字以外の時は、東日本部門）
     IF      SGY-F95  NOT  NUMERIC
             MOVE    WK-BUMON    TO   TO0-F01
     ELSE
             MOVE    SGY-F95     TO   TO0-F01
     END-IF.
*    伝票区分
     MOVE    30                  TO   TO0-F02.
*    伝票_
     MOVE    ZERO                TO   TO0-F03(1:2).
     MOVE    SGY-F01             TO   TO0-F03(3:7).
*    行_
     MOVE    SGY-F02             TO   TO0-F04.
*    出荷日
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
     MOVE    SYSYMD2             TO   TO0-F27.
*   __
     MOVE    SGY-F10             TO   TO0-F32.
     MOVE    SGY-F14             TO   TO0-F25.
*   計上ファイル追加
     WRITE                       TOK-REC.
*   計上フラグ更新（計上済）
     MOVE    1                   TO   SGY-F13.
     REWRITE                     SGY-REC.
*   作業実績ファイル読込み
     PERFORM      SGYO-READ.
 220-SGYO-SHORI-EXT.
     EXIT.
************************************************************
*      9000      入出庫ファイルの読込処理                  *
************************************************************
 NYUS-READ              SECTION.
*    入出庫ファイル読込み
     READ    NYSFILF   NEXT
        AT   END
             MOVE      "END"     TO   EOF-FLG1
             GO        TO        NYUS-READ-EXT
     END-READ.
*    作業区分チェック
     IF      NYU-F04   =         "01"
             GO        TO        NYUS-READ
     END-IF.
*    計上フラグチェック
     IF      NYU-F14   NOT  =    ZERO
             GO        TO        NYUS-READ
     END-IF.
*    削除フラグチェック
     IF      NYU-F96   NOT  =    SPACE
             GO        TO        NYUS-READ
     END-IF.
*    出荷場所－入荷場所チェック
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
*    作業実績ファイル読込み
     READ    SGYFILF   NEXT
        AT   END
             MOVE      "END"     TO   EOF-FLG2
             GO        TO        SGYO-READ-EXT
     END-READ.
*    計上フラグチェック
     IF      SGY-F13   NOT  =    ZERO
             GO        TO        SGYO-READ
     END-IF.
*    削除フラグチェック
     IF      SGY-F97   NOT  =    ZERO
             GO        TO        SGYO-READ
     END-IF.
 SGYO-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC            SECTION.
     CLOSE        NYSFILF   SGYFILF   TOKU.
 300-END-SEC-EXT.
     EXIT.
*--------------------------------------------------------------*
*    条件マスタ読込み
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "20"                 TO   JYO-F01.
     MOVE    WK-SOKCD             TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              DISPLAY NC"＃＃条件Ｆ異常＃＃" UPON  CONS
              DISPLAY NC"＃＃部門ＣＨＫ＃＃" UPON  CONS
              MOVE "INV"          TO   HJYOKEN-INV-FLG
**************GO                  TO   HJYOKEN-READ-EXIT
       NOT INVALID KEY
              MOVE SPACE          TO   HJYOKEN-INV-FLG
              MOVE JYO-F05        TO   WK-BUMON-CD
     END-READ.
*****REWRITE  JYO-REC.
 HJYOKEN-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
